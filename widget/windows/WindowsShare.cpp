#include "WindowsShare.h"

#include "mozilla/WindowsVersion.h"
#include "mozilla/media/MediaUtils.h"

#include <windows.h>
#include <winsdkver.h>
#include <wrl.h>

/* mingw currently doesn't support windows.ui.viewmanagement.h, so we disable it
 * until it's fixed. */
#ifndef __MINGW32__

#  include <windows.ui.viewmanagement.h>
#  pragma comment(lib, "runtimeobject.lib")

#endif

/* mingw currently doesn't support windows.ui.viewmanagement.h, so we disable it
 * until it's fixed. */
#ifndef __MINGW32__

#  ifndef __IDataTransferManagerInterop_INTERFACE_DEFINED__
#    define __IDataTransferManagerInterop_INTERFACE_DEFINED__

typedef interface IDataTransferManagerInterop IDataTransferManagerInterop;

MIDL_INTERFACE("3A3DCD6C-3EAB-43DC-BCDE-45671CE800C8")
IDataTransferManagerInterop : public IUnknown {
 public:
  virtual HRESULT STDMETHODCALLTYPE GetForWindow(
      HWND appWindow, REFIID riid, void** dataTransferManager) = 0;
  virtual HRESULT STDMETHODCALLTYPE ShowShareUIForWindow(HWND appWindow) = 0;
};

#  endif

#endif

using namespace mozilla;
using namespace ABI::Windows::UI;
using namespace ABI::Windows::UI::ViewManagement;
using namespace Microsoft::WRL;
using namespace Microsoft::WRL::Wrappers;
using namespace ABI::Windows::Foundation;
using namespace ABI::Windows::ApplicationModel::DataTransfer;

struct HStringDeleter {
  typedef HSTRING pointer;
  void operator()(pointer aString) { WindowsDeleteString(aString); }
};

typedef mozilla::UniquePtr<HSTRING, HStringDeleter> HStringUniquePtr;

Result<HStringUniquePtr, HRESULT> ConvertToWindowsString(
    const nsAString& aStr) {
  HSTRING rawStr;
  HRESULT hr = WindowsCreateString(PromiseFlatString(aStr).get(), aStr.Length(),
                                   &rawStr);
  if (FAILED(hr)) {
    return Err(hr);
  }
  return HStringUniquePtr(rawStr);
}

Result<Ok, nsresult> RequestShare(
    std::function<HRESULT(IDataRequestedEventArgs* pArgs)> aCallback) {
  if (!IsWin10OrLater()) {
    return Err(NS_ERROR_FAILURE);
  }

  HWND hwnd = GetForegroundWindow();
  if (!hwnd) {
    return Err(NS_ERROR_FAILURE);
  }

  ComPtr<IDataTransferManagerInterop> dtmInterop;
  HRESULT hr = RoGetActivationFactory(
      HStringReference(
          RuntimeClass_Windows_ApplicationModel_DataTransfer_DataTransferManager)
          .Get(),
      IID_PPV_ARGS(&dtmInterop));
  if (FAILED(hr)) {
    return Err(NS_ERROR_FAILURE);
  }

  ComPtr<IDataTransferManager> dtm;
  hr = dtmInterop->GetForWindow(hwnd, IID_PPV_ARGS(&dtm));
  if (FAILED(hr)) {
    return Err(NS_ERROR_FAILURE);
  }

  auto callback = Callback<
      ITypedEventHandler<DataTransferManager*, DataRequestedEventArgs*>>(
      [aCallback](IDataTransferManager*,
                  IDataRequestedEventArgs* pArgs) -> HRESULT {
        return aCallback(pArgs);
      });

  EventRegistrationToken dataRequestedToken;
  hr = dtm->add_DataRequested(callback.Get(), &dataRequestedToken);
  if (FAILED(hr)) {
    return Err(NS_ERROR_FAILURE);
  }

  hr = dtmInterop->ShowShareUIForWindow(hwnd);
  if (FAILED(hr)) {
    return Err(NS_ERROR_FAILURE);
  }

  return Ok();
}

RefPtr<SharePromise> WindowsShare::Share(nsAutoString aTitle,
                                         nsAutoString aText,
                                         nsAutoString aUrl) {
  auto holder = MakeRefPtr<
      mozilla::media::Refcountable<MozPromiseHolder<SharePromise>>>();
  RefPtr<SharePromise> promise = holder->Ensure(__func__);

#ifndef __MINGW32__
  auto result = RequestShare([holder, title = std::move(aTitle),
                              text = std::move(aText), url = std::move(aUrl)](
                                 IDataRequestedEventArgs* pArgs) {
    ComPtr<IDataRequest> spDataRequest;
    HRESULT hr = pArgs->get_Request(&spDataRequest);
    if (FAILED(hr)) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return hr;
    }

    ComPtr<IDataPackage> spDataPackage;
    hr = spDataRequest->get_Data(&spDataPackage);
    if (FAILED(hr)) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return hr;
    }

    ComPtr<IDataPackage2> spDataPackage2;
    hr = spDataPackage->QueryInterface(IID_PPV_ARGS(&spDataPackage2));
    if (FAILED(hr)) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return hr;
    }

    ComPtr<IDataPackage3> spDataPackage3;
    hr = spDataPackage->QueryInterface(IID_PPV_ARGS(&spDataPackage3));
    if (FAILED(hr)) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return hr;
    }

    ComPtr<IDataPackage4> spDataPackage4;
    hr = spDataPackage->QueryInterface(IID_PPV_ARGS(&spDataPackage4));
    if (FAILED(hr)) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return hr;
    }

    ComPtr<IDataPackagePropertySet> spDataPackageProperties;
    hr = spDataPackage->get_Properties(&spDataPackageProperties);
    if (FAILED(hr)) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return hr;
    }

    auto wTitle =
        ConvertToWindowsString((title.IsVoid() || title.Length() == 0)
                                   ? nsAutoString(NS_LITERAL_STRING(" "))
                                   : title);
    if (wTitle.isErr()) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return wTitle.unwrapErr();
    }
    hr = spDataPackageProperties->put_Title(wTitle.unwrap().get());
    if (FAILED(hr)) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return hr;
    }

    // Assign even if empty, as Windows requires some data to share
    auto wText = ConvertToWindowsString(text);
    if (wText.isErr()) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return wText.unwrapErr();
    }
    hr = spDataPackage->SetText(wText.unwrap().get());
    if (FAILED(hr)) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return hr;
    }

    if (!url.IsVoid()) {
      auto wUrl = ConvertToWindowsString(url);
      if (wUrl.isErr()) {
        holder->Reject(NS_ERROR_FAILURE, __func__);
        return wUrl.unwrapErr();
      }

      ComPtr<IUriRuntimeClassFactory> uriFactory;
      hr = GetActivationFactory(
          HStringReference(RuntimeClass_Windows_Foundation_Uri).Get(),
          &uriFactory);
      if (FAILED(hr)) {
        return hr;
      }

      ComPtr<IUriRuntimeClass> uri;
      hr = uriFactory->CreateUri(wUrl.unwrap().get(), &uri);
      if (FAILED(hr)) {
        holder->Reject(NS_ERROR_FAILURE, __func__);
        return hr;
      }
      hr = spDataPackage2->SetWebLink(uri.Get());
      if (FAILED(hr)) {
        holder->Reject(NS_ERROR_FAILURE, __func__);
        return hr;
      }
    }

    auto completedCallback =
        Callback<ITypedEventHandler<DataPackage*, ShareCompletedEventArgs*>>(
            [holder](IDataPackage*, IShareCompletedEventArgs*) -> HRESULT {
              holder->Resolve(true, __func__);
              return S_OK;
            });

    EventRegistrationToken dataRequestedToken;
    hr = spDataPackage3->add_ShareCompleted(completedCallback.Get(),
                                            &dataRequestedToken);
    if (FAILED(hr)) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return hr;
    }

    auto canceledCallback =
        Callback<ITypedEventHandler<DataPackage*, IInspectable*>>(
            [holder](IDataPackage*, IInspectable*) -> HRESULT {
              holder->Reject(NS_ERROR_FAILURE, __func__);
              return S_OK;
            });

    hr = spDataPackage4->add_ShareCanceled(canceledCallback.Get(),
                                           &dataRequestedToken);
    if (FAILED(hr)) {
      holder->Reject(NS_ERROR_FAILURE, __func__);
      return hr;
    }

    return S_OK;
  });
  if (result.isErr()) {
    holder->Reject(result.unwrapErr(), __func__);
  }
#else
  holder->Reject(NS_ERROR_FAILURE, __func__);
  return promise;
#endif

  return promise;
}
