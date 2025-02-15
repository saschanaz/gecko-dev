/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=8 sts=2 et sw=2 tw=80: */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */
#include "WinContentSystemParameters.h"
#include "WinUtils.h"
#include "mozilla/Mutex.h"
#include "mozilla/dom/ContentChild.h"

namespace mozilla {
namespace widget {
using dom::SystemParameterKVPair;

static_assert(uint8_t(SystemParameterId::Count) < 32,
              "Too many SystemParameterId for "
              "WinContentSystemParameters::Detail::validCachedValueBitfield");

struct WinContentSystemParameters::Detail {
  OffTheBooksMutex mutex{"WinContentSystemParameters::Detail::mutex"};
  // A bitfield indicating which cached ids are valid
  uint32_t validCachedValueBitfield{0};
  bool cachedIsPerMonitorDPIAware{false};
  float cachedSystemDPI{0.0f};
};

// static
WinContentSystemParameters* WinContentSystemParameters::GetSingleton() {
  static WinContentSystemParameters sInstance{};
  return &sInstance;
}

WinContentSystemParameters::WinContentSystemParameters()
    : mDetail(std::make_unique<Detail>()) {}

WinContentSystemParameters::~WinContentSystemParameters() {}

bool WinContentSystemParameters::IsPerMonitorDPIAware() {
  MOZ_ASSERT(XRE_IsContentProcess());

  OffTheBooksMutexAutoLock lock(mDetail->mutex);
  MOZ_RELEASE_ASSERT(
      IsCachedValueValid(SystemParameterId::IsPerMonitorDPIAware));
  return mDetail->cachedIsPerMonitorDPIAware;
}

float WinContentSystemParameters::SystemDPI() {
  MOZ_ASSERT(XRE_IsContentProcess());

  OffTheBooksMutexAutoLock lock(mDetail->mutex);
  MOZ_RELEASE_ASSERT(IsCachedValueValid(SystemParameterId::SystemDPI));
  return mDetail->cachedSystemDPI;
}

void WinContentSystemParameters::SetContentValueInternal(
    const SystemParameterKVPair& aKVPair) {
  MOZ_ASSERT(XRE_IsContentProcess());
  MOZ_ASSERT(NS_IsMainThread());

  MOZ_RELEASE_ASSERT(aKVPair.id() < uint8_t(SystemParameterId::Count));

  SetCachedValueValid(SystemParameterId(aKVPair.id()), true);

  mDetail->mutex.AssertCurrentThreadOwns();

  switch (SystemParameterId(aKVPair.id())) {
    case SystemParameterId::IsPerMonitorDPIAware:
      mDetail->cachedIsPerMonitorDPIAware = aKVPair.value();
      return;

    case SystemParameterId::SystemDPI:
      mDetail->cachedSystemDPI = aKVPair.value();
      return;

    case SystemParameterId::Count:
      MOZ_CRASH("Invalid SystemParameterId");
  }
  MOZ_CRASH("Unhandled SystemParameterId");
}

void WinContentSystemParameters::SetContentValue(
    const SystemParameterKVPair& aKVPair) {
  MOZ_ASSERT(XRE_IsContentProcess());
  MOZ_ASSERT(NS_IsMainThread());

  OffTheBooksMutexAutoLock lock(mDetail->mutex);
  SetContentValueInternal(aKVPair);
}

void WinContentSystemParameters::SetContentValues(
    const nsTArray<dom::SystemParameterKVPair>& values) {
  MOZ_ASSERT(XRE_IsContentProcess());
  MOZ_ASSERT(NS_IsMainThread());

  OffTheBooksMutexAutoLock lock(mDetail->mutex);
  for (auto& kvPair : values) {
    SetContentValueInternal(kvPair);
  }
}

bool WinContentSystemParameters::GetParentValueInternal(
    uint8_t aId, SystemParameterKVPair* aKVPair) {
  MOZ_ASSERT(XRE_IsParentProcess());
  MOZ_ASSERT(aId < uint8_t(SystemParameterId::Count));
  MOZ_ASSERT(aKVPair);

  aKVPair->id() = aId;

  switch (SystemParameterId(aId)) {
    case SystemParameterId::IsPerMonitorDPIAware:
      aKVPair->value() = WinUtils::IsPerMonitorDPIAware();
      return true;

    case SystemParameterId::SystemDPI:
      aKVPair->value() = WinUtils::SystemDPI();
      return true;

    case SystemParameterId::Count:
      MOZ_CRASH("Invalid SystemParameterId");
  }
  MOZ_CRASH("Unhandled SystemParameterId");
}

bool WinContentSystemParameters::GetParentValue(
    uint8_t aId, dom::SystemParameterKVPair* aKVPair) {
  MOZ_ASSERT(XRE_IsParentProcess());
  MOZ_RELEASE_ASSERT(aId < uint8_t(SystemParameterId::Count));
  MOZ_RELEASE_ASSERT(aKVPair);
  return GetParentValueInternal(aId, aKVPair);
}

nsTArray<dom::SystemParameterKVPair>
WinContentSystemParameters::GetParentValues() {
  MOZ_ASSERT(XRE_IsParentProcess());

  nsTArray<dom::SystemParameterKVPair> results;
  for (uint8_t i = 0; i < uint8_t(SystemParameterId::Count); ++i) {
    dom::SystemParameterKVPair kvPair{};
    GetParentValueInternal(i, &kvPair);
    results.AppendElement(std::move(kvPair));
  }
  return results;
}

bool WinContentSystemParameters::IsCachedValueValid(SystemParameterId aId) {
  MOZ_ASSERT(XRE_IsContentProcess());
  MOZ_ASSERT(uint8_t(aId) < uint8_t(SystemParameterId::Count));
  mDetail->mutex.AssertCurrentThreadOwns();
  uint32_t mask = uint32_t(1) << uint8_t(aId);
  return (mDetail->validCachedValueBitfield & mask) != 0;
}

void WinContentSystemParameters::SetCachedValueValid(SystemParameterId aId,
                                                     bool aIsValid) {
  MOZ_ASSERT(XRE_IsContentProcess());
  MOZ_ASSERT(NS_IsMainThread());
  MOZ_ASSERT(uint8_t(aId) < uint8_t(SystemParameterId::Count));
  mDetail->mutex.AssertCurrentThreadOwns();
  uint32_t mask = uint32_t(1) << uint8_t(aId);

  if (aIsValid) {
    mDetail->validCachedValueBitfield |= mask;
  } else {
    mDetail->validCachedValueBitfield &= ~mask;
  }
}

}  // namespace widget
}  // namespace mozilla