/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef mozilla_widget_WindowsShare_h__
#define mozilla_widget_WindowsShare_h__

#include "mozilla/MozPromise.h"
#include "nsString.h"

typedef mozilla::MozPromise<bool, nsresult, true> SharePromise;

class WindowsShare final {
 public:
  static RefPtr<SharePromise> Share(nsAutoString aTitle, nsAutoString aText,
                                    nsAutoString aUrl);
};

#endif  // mozilla_widget_WindowsShare_h__
