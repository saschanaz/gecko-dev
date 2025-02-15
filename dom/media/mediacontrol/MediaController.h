/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=8 sts=2 et sw=2 tw=80: */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef DOM_MEDIA_MEDIACONTROL_MEDIACONTROLLER_H_
#define DOM_MEDIA_MEDIACONTROL_MEDIACONTROLLER_H_

#include "MediaEventSource.h"
#include "MediaPlaybackStatus.h"
#include "MediaStatusManager.h"
#include "mozilla/DOMEventTargetHelper.h"
#include "mozilla/dom/MediaControllerBinding.h"
#include "mozilla/LinkedList.h"
#include "nsDataHashtable.h"
#include "nsISupportsImpl.h"

namespace mozilla {
namespace dom {

class BrowsingContext;

/**
 * IMediaController is an interface which includes control related methods and
 * methods used to know its playback state.
 */
class IMediaController {
 public:
  NS_INLINE_DECL_PURE_VIRTUAL_REFCOUNTING

  // Focus the window currently playing media.
  virtual void Focus() = 0;
  virtual void Play() = 0;
  virtual void Pause() = 0;
  virtual void Stop() = 0;
  virtual void PrevTrack() = 0;
  virtual void NextTrack() = 0;
  virtual void SeekBackward() = 0;
  virtual void SeekForward() = 0;

  // Return the ID of the top level browsing context within a tab.
  virtual uint64_t Id() const = 0;
  virtual bool IsAudible() const = 0;
  virtual bool IsPlaying() const = 0;
};

/**
 * MediaController is a class, which is used to control all media within a tab.
 * It can only be used in Chrome process and the controlled media are usually
 * in the content process (unless we disable e10s).
 *
 * Each tab would have only one media controller, they are 1-1 corresponding
 * relationship, we use tab's top-level browsing context ID to initialize the
 * controller and use that as its ID.
 *
 * The controller would be activated when its controlled media starts and
 * becomes audible. After the controller is activated, then we can use its
 * controlling methods, such as `Play()`, `Pause()` to control the media within
 * the tab.
 *
 * If there is at least one controlled media playing in the tab, then we would
 * say the controller is `playing`. If there is at least one controlled media is
 * playing and audible, then we would say the controller is `audible`.
 *
 * Note that, if we don't enable audio competition, then we might have multiple
 * tabs playing media at the same time, we can use the ID to query the specific
 * controller from `MediaControlService`.
 */
class MediaController final : public DOMEventTargetHelper,
                              public IMediaController,
                              public LinkedListElement<RefPtr<MediaController>>,
                              public MediaStatusManager {
 public:
  NS_DECL_ISUPPORTS_INHERITED
  NS_DECL_CYCLE_COLLECTION_SCRIPT_HOLDER_CLASS_INHERITED(MediaController,
                                                         DOMEventTargetHelper)
  explicit MediaController(uint64_t aBrowsingContextId);

  // WebIDL methods
  nsISupports* GetParentObject() const;
  JSObject* WrapObject(JSContext* aCx,
                       JS::Handle<JSObject*> aGivenProto) override;
  void GetSupportedKeys(nsTArray<MediaControlKey>& aRetVal) const;
  IMPL_EVENT_HANDLER(supportedkeyschange);

  // IMediaController's methods
  void Focus() override;
  void Play() override;
  void Pause() override;
  void Stop() override;
  void PrevTrack() override;
  void NextTrack() override;
  void SeekBackward() override;
  void SeekForward() override;
  uint64_t Id() const override;
  bool IsAudible() const override;
  bool IsPlaying() const override;

  // IMediaInfoUpdater's methods
  void NotifyMediaPlaybackChanged(uint64_t aBrowsingContextId,
                                  MediaPlaybackState aState) override;
  void NotifyMediaAudibleChanged(uint64_t aBrowsingContextId,
                                 MediaAudibleState aState) override;
  void SetIsInPictureInPictureMode(uint64_t aBrowsingContextId,
                                   bool aIsInPictureInPictureMode) override;

  // Reture true if any of controlled media is being used in Picture-In-Picture
  // mode.
  bool IsInPictureInPictureMode() const;

  // Calling this method explicitly would mark this controller as deprecated,
  // then calling any its method won't take any effect.
  void Shutdown();

  // This event would be notified media controller's supported media keys
  // change.
  MediaEventSource<nsTArray<MediaControlKey>>& SupportedKeysChangedEvent() {
    return mSupportedKeysChangedEvent;
  }

  CopyableTArray<MediaControlKey> GetSupportedMediaKeys() const;

 private:
  ~MediaController();
  void HandleActualPlaybackStateChanged() override;
  void UpdateMediaControlKeyToContentMediaIfNeeded(MediaControlKey aKey);
  void HandleSupportedMediaSessionActionsChanged(
      const nsTArray<MediaSessionAction>& aSupportedAction);

  // This would register controller to the media control service that takes a
  // responsibility to manage all active controllers.
  void Activate();

  // This would unregister controller from the media control service.
  void Deactivate();

  void UpdateActivatedStateIfNeeded();
  bool ShouldActivateController() const;
  bool ShouldDeactivateController() const;

  bool mIsRegisteredToService = false;
  bool mShutdown = false;
  bool mIsInPictureInPictureMode = false;

  // We would monitor the change of media session actions and convert them to
  // the media keys, then determine the supported media keys.
  MediaEventListener mSupportedActionsChangedListener;
  MediaEventProducer<nsTArray<MediaControlKey>> mSupportedKeysChangedEvent;
  // Use copyable array so that we can use the result as a parameter for the
  // media event.
  CopyableTArray<MediaControlKey> mSupportedKeys;
};

}  // namespace dom
}  // namespace mozilla

#endif
