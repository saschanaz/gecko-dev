/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set sw=2 ts=8 et tw=80 : */

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "ParentProcessDocumentChannel.h"

#include "mozilla/StaticPrefs_extensions.h"
#include "nsDocShell.h"
#include "nsIObserverService.h"
#include "nsIClassifiedChannel.h"
#include "nsIRedirectChannelRegistrar.h"
#include "mozilla/net/UrlClassifierCommon.h"

extern mozilla::LazyLogModule gDocumentChannelLog;
#define LOG(fmt) MOZ_LOG(gDocumentChannelLog, mozilla::LogLevel::Verbose, fmt)

namespace mozilla {
namespace net {

using RedirectToRealChannelPromise =
    typename PDocumentChannelParent::RedirectToRealChannelPromise;

class ParentChannelWrapper : public nsIParentChannel {
 public:
  ParentChannelWrapper(nsIChannel* aChannel, nsIStreamListener* aListener)
      : mChannel(aChannel), mListener(aListener) {}

  NS_DECL_ISUPPORTS
  NS_DECL_NSIPARENTCHANNEL
  NS_FORWARD_NSISTREAMLISTENER(mListener->)
  NS_FORWARD_NSIREQUESTOBSERVER(mListener->)

 private:
  virtual ~ParentChannelWrapper() = default;
  const nsCOMPtr<nsIChannel> mChannel;
  const nsCOMPtr<nsIStreamListener> mListener;
};

NS_IMPL_ISUPPORTS(ParentChannelWrapper, nsIParentChannel, nsIStreamListener,
                  nsIRequestObserver);

NS_IMPL_ISUPPORTS_INHERITED(ParentProcessDocumentChannel, DocumentChannel,
                            nsIAsyncVerifyRedirectCallback, nsIObserver)

ParentProcessDocumentChannel::ParentProcessDocumentChannel(
    nsDocShellLoadState* aLoadState, class LoadInfo* aLoadInfo,
    nsLoadFlags aLoadFlags, uint32_t aCacheKey, bool aUriModified,
    bool aIsXFOError)
    : DocumentChannel(aLoadState, aLoadInfo, aLoadFlags, aCacheKey,
                      aUriModified, aIsXFOError) {
  LOG(("ParentProcessDocumentChannel ctor [this=%p]", this));
}

ParentProcessDocumentChannel::~ParentProcessDocumentChannel() {
  LOG(("ParentProcessDocumentChannel dtor [this=%p]", this));
}

RefPtr<RedirectToRealChannelPromise>
ParentProcessDocumentChannel::RedirectToRealChannel(
    nsTArray<ipc::Endpoint<extensions::PStreamFilterParent>>&&
        aStreamFilterEndpoints,
    uint32_t aRedirectFlags, uint32_t aLoadFlags) {
  LOG(("ParentProcessDocumentChannel RedirectToRealChannel [this=%p]", this));
  nsCOMPtr<nsIChannel> channel = mDocumentLoadListener->GetChannel();
  channel->SetLoadFlags(aLoadFlags);
  channel->SetNotificationCallbacks(mCallbacks);

  if (mLoadGroup) {
    channel->SetLoadGroup(mLoadGroup);
  }

  if (XRE_IsE10sParentProcess()) {
    nsCOMPtr<nsIURI> uri;
    MOZ_ALWAYS_SUCCEEDS(NS_GetFinalChannelURI(channel, getter_AddRefs(uri)));
    if (!nsDocShell::CanLoadInParentProcess(uri)) {
      nsAutoCString msg;
      uri->GetSpec(msg);
      msg.Insert(
          "Attempt to load a non-authorised load in the parent process: ", 0);
      NS_ASSERTION(false, msg.get());
      return RedirectToRealChannelPromise::CreateAndResolve(
          NS_ERROR_CONTENT_BLOCKED, __func__);
    }
  }
  mStreamFilterEndpoints = std::move(aStreamFilterEndpoints);

  RefPtr<RedirectToRealChannelPromise> p = mPromise.Ensure(__func__);
  // We make the promise use direct task dispatch in order to reduce the number
  // of event loops iterations.
  mPromise.UseDirectTaskDispatch(__func__);

  nsresult rv =
      gHttpHandler->AsyncOnChannelRedirect(this, channel, aRedirectFlags);
  if (NS_FAILED(rv)) {
    LOG(
        ("ParentProcessDocumentChannel RedirectToRealChannel "
         "AsyncOnChannelRedirect failed [this=%p "
         "aRv=%d]",
         this, int(rv)));
    OnRedirectVerifyCallback(rv);
  }

  return p;
}

NS_IMETHODIMP
ParentProcessDocumentChannel::OnRedirectVerifyCallback(nsresult aResult) {
  LOG(
      ("ParentProcessDocumentChannel OnRedirectVerifyCallback [this=%p "
       "aResult=%d]",
       this, int(aResult)));

  MOZ_ASSERT(mDocumentLoadListener);

  if (NS_FAILED(aResult)) {
    Cancel(aResult);
  } else if (mCanceled) {
    aResult = NS_ERROR_ABORT;
  } else {
    const nsCOMPtr<nsIChannel> channel = mDocumentLoadListener->GetChannel();
    mLoadGroup->AddRequest(channel, nullptr);
    // Adding the channel to the loadgroup could have triggered a status
    // change with an observer being called destroying the docShell, resulting
    // in the PPDC to be canceled.
    if (mCanceled) {
      aResult = NS_ERROR_ABORT;
    } else {
      mLoadGroup->RemoveRequest(this, nullptr, NS_BINDING_REDIRECTED);
      for (auto& endpoint : mStreamFilterEndpoints) {
        extensions::StreamFilterParent::Attach(channel, std::move(endpoint));
      }

      RefPtr<ParentChannelWrapper> wrapper =
          new ParentChannelWrapper(channel, mListener);

      nsCOMPtr<nsIRedirectChannelRegistrar> registrar =
          RedirectChannelRegistrar::GetOrCreate();
      nsCOMPtr<nsIChannel> dummy;
      MOZ_ALWAYS_SUCCEEDS(
          NS_LinkRedirectChannels(mDocumentLoadListener->GetRedirectChannelId(),
                                  wrapper, getter_AddRefs(dummy)));
      MOZ_ASSERT(dummy == channel);
    }
  }

  mPromise.Resolve(aResult, __func__);

  return NS_OK;
}

NS_IMETHODIMP ParentProcessDocumentChannel::AsyncOpen(
    nsIStreamListener* aListener) {
  LOG(("ParentProcessDocumentChannel AsyncOpen [this=%p]", this));
  mDocumentLoadListener = new DocumentLoadListener(
      GetDocShell()->GetBrowsingContext()->Canonical());
  LOG(("Created PPDocumentChannel with listener=%p",
       mDocumentLoadListener.get()));

  // Add observers.
  nsCOMPtr<nsIObserverService> observerService =
      mozilla::services::GetObserverService();
  if (observerService) {
    MOZ_ALWAYS_SUCCEEDS(observerService->AddObserver(
        this, NS_HTTP_ON_MODIFY_REQUEST_TOPIC, false));
  }

  gHttpHandler->OnOpeningDocumentRequest(this);

  nsresult rv = NS_OK;
  Maybe<dom::ClientInfo> initialClientInfo = mInitialClientInfo;
  auto promise = mDocumentLoadListener->Open(
      mLoadState, mCacheKey, Some(mChannelId), mAsyncOpenTime, mTiming,
      std::move(initialClientInfo), GetDocShell()->GetOuterWindowID(),
      GetDocShell()
          ->GetBrowsingContext()
          ->HasValidTransientUserGestureActivation(),
      Some(mUriModified), Some(mIsXFOError), 0 /* ProcessId */, &rv);
  if (NS_FAILED(rv)) {
    MOZ_ASSERT(!promise);
    mDocumentLoadListener = nullptr;
    RemoveObserver();
    return rv;
  }

  mListener = aListener;
  if (mLoadGroup) {
    mLoadGroup->AddRequest(this, nullptr);
  }

  RefPtr<ParentProcessDocumentChannel> self = this;
  promise->Then(
      GetCurrentThreadSerialEventTarget(), __func__,
      [self](DocumentLoadListener::OpenPromiseSucceededType&& aResolveValue) {
        // The DLL is waiting for us to resolve the
        // RedirectToRealChannelPromise given as parameter.
        RefPtr<RedirectToRealChannelPromise> p =
            self->RedirectToRealChannel(
                    std::move(aResolveValue.mStreamFilterEndpoints),
                    aResolveValue.mRedirectFlags, aResolveValue.mLoadFlags)
                ->Then(
                    GetCurrentThreadSerialEventTarget(), __func__,
                    [self](RedirectToRealChannelPromise::ResolveOrRejectValue&&
                               aValue) -> RefPtr<RedirectToRealChannelPromise> {
                      MOZ_ASSERT(aValue.IsResolve());
                      nsresult rv = aValue.ResolveValue();
                      if (NS_FAILED(rv)) {
                        self->DisconnectChildListeners(rv, rv);
                      }
                      self->mLoadGroup = nullptr;
                      self->mListener = nullptr;
                      self->mCallbacks = nullptr;
                      self->RemoveObserver();
                      auto p =
                          MakeRefPtr<RedirectToRealChannelPromise::Private>(
                              __func__);
                      p->UseDirectTaskDispatch(__func__);
                      p->ResolveOrReject(std::move(aValue), __func__);
                      return p;
                    });
        // We chain the promise the DLL is waiting on to the one returned by
        // RedirectToRealChannel. As soon as the promise returned is
        // resolved or rejected, so will the DLL's promise.
        p->ChainTo(aResolveValue.mPromise.forget(), __func__);
      },
      [self](DocumentLoadListener::OpenPromiseFailedType&& aRejectValue) {
        self->DisconnectChildListeners(aRejectValue.mStatus,
                                       aRejectValue.mLoadGroupStatus);
        self->RemoveObserver();
      });
  return NS_OK;
}

NS_IMETHODIMP ParentProcessDocumentChannel::Cancel(nsresult aStatus) {
  LOG(("ParentProcessDocumentChannel Cancel [this=%p]", this));
  if (mCanceled) {
    return NS_OK;
  }

  mCanceled = true;
  // This will force the DocumentListener to abort the promise if there's one
  // pending.
  mDocumentLoadListener->Cancel(aStatus);

  return NS_OK;
}

void ParentProcessDocumentChannel::RemoveObserver() {
  if (nsCOMPtr<nsIObserverService> observerService =
          mozilla::services::GetObserverService()) {
    observerService->RemoveObserver(this, NS_HTTP_ON_MODIFY_REQUEST_TOPIC);
  }
}

////////////////////////////////////////////////////////////////////////////////
// nsIObserver
////////////////////////////////////////////////////////////////////////////////

NS_IMETHODIMP
ParentProcessDocumentChannel::Observe(nsISupports* aSubject, const char* aTopic,
                                      const char16_t* aData) {
  MOZ_ASSERT(NS_IsMainThread());

  if (mRequestObserversCalled) {
    // We have already emitted the event, we don't want to emit it again.
    // We only care about forwarding the first NS_HTTP_ON_MODIFY_REQUEST_TOPIC
    // encountered.
    return NS_OK;
  }
  nsCOMPtr<nsIChannel> channel = do_QueryInterface(aSubject);
  if (!channel || mDocumentLoadListener->GetChannel() != channel) {
    // Not a channel we are interested with.
    return NS_OK;
  }
  LOG(("DocumentChannelParent Observe [this=%p aChannel=%p]", this,
       channel.get()));
  if (!nsCRT::strcmp(aTopic, NS_HTTP_ON_MODIFY_REQUEST_TOPIC)) {
    mRequestObserversCalled = true;
    gHttpHandler->OnModifyDocumentRequest(this);
  }

  return NS_OK;
}

////////////////////////////////////////////////////////////////////////////////
// nsIParentChannel
////////////////////////////////////////////////////////////////////////////////

NS_IMETHODIMP
ParentChannelWrapper::SetParentListener(
    mozilla::net::ParentChannelListener* listener) {
  return NS_OK;
}

NS_IMETHODIMP
ParentChannelWrapper::NotifyFlashPluginStateChanged(
    nsIHttpChannel::FlashPluginState aState) {
  // For now, only HttpChannel use this attribute.
  RefPtr<HttpBaseChannel> httpChannel = do_QueryObject(mChannel.get());
  if (httpChannel) {
    httpChannel->SetFlashPluginState(aState);
  }
  return NS_OK;
}

NS_IMETHODIMP
ParentChannelWrapper::SetClassifierMatchedInfo(const nsACString& aList,
                                               const nsACString& aProvider,
                                               const nsACString& aFullHash) {
  nsCOMPtr<nsIClassifiedChannel> classifiedChannel =
      do_QueryInterface(mChannel);
  if (classifiedChannel) {
    classifiedChannel->SetMatchedInfo(aList, aProvider, aFullHash);
  }
  return NS_OK;
}

NS_IMETHODIMP
ParentChannelWrapper::SetClassifierMatchedTrackingInfo(
    const nsACString& aLists, const nsACString& aFullHash) {
  nsCOMPtr<nsIClassifiedChannel> classifiedChannel =
      do_QueryInterface(mChannel);
  if (classifiedChannel) {
    nsTArray<nsCString> lists, fullhashes;
    for (const nsACString& token : aLists.Split(',')) {
      lists.AppendElement(token);
    }
    for (const nsACString& token : aFullHash.Split(',')) {
      fullhashes.AppendElement(token);
    }
    classifiedChannel->SetMatchedTrackingInfo(lists, fullhashes);
  }
  return NS_OK;
}

NS_IMETHODIMP
ParentChannelWrapper::NotifyClassificationFlags(uint32_t aClassificationFlags,
                                                bool aIsThirdParty) {
  UrlClassifierCommon::SetClassificationFlagsHelper(
      mChannel, aClassificationFlags, aIsThirdParty);
  return NS_OK;
}

NS_IMETHODIMP
ParentChannelWrapper::Delete() { return NS_OK; }

}  // namespace net
}  // namespace mozilla

#undef LOG
