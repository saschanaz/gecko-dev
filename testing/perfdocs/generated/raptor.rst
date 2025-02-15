######
Raptor
######

The following documents all testing we have for Raptor.

Benchmarks
----------
Standard benchmarks are third-party tests (i.e. Speedometer) that we have integrated into Raptor to run per-commit in our production CI. 


Desktop
-------
Tests for page-load performance. (WX: WebExtension, BT: Browsertime, FF: Firefox, CH: Chrome, CU: Chromium)

* amazon (BT)
* bing-search (BT)
* facebook (BT)
* google-search (BT)
* google-slides (BT)
* raptor-tp6-amazon (WX, FF, CH, CU)
* raptor-tp6-apple (WX, FF, CH, CU)
* raptor-tp6-binast-instagram-firefox (WX, FF)
* raptor-tp6-bing (WX, FF, CH, CU)
* raptor-tp6-docs (WX, FF, CH, CU)
* raptor-tp6-ebay (WX, FF, CH, CU)
* raptor-tp6-facebook (WX, CH, CU)
* raptor-tp6-fandom (WX, FF, CH, CU)
* raptor-tp6-google (WX, FF, CH, CU)
* raptor-tp6-google-mail (WX, FF, CH, CU)
* raptor-tp6-imdb (WX, FF, CH, CU)
* raptor-tp6-imgur (WX, FF, CH, CU)
* raptor-tp6-instagram (WX, FF, CH, CU)
* raptor-tp6-linkedin (WX, FF, CH, CU)
* raptor-tp6-microsoft (WX, FF, CH, CU)
* raptor-tp6-netflix (WX, FF, CH, CU)
* raptor-tp6-office (WX, FF, CH, CU)
* raptor-tp6-outlook (WX, FF, CH, CU)
* raptor-tp6-paypal (WX, FF, CH, CU)
* raptor-tp6-pinterest (WX, FF, CH, CU)
* raptor-tp6-reddit (WX, FF, CH, CU)
* raptor-tp6-sheets (WX, FF, CH, CU)
* raptor-tp6-slides (WX, FF, CH, CU)
* raptor-tp6-tumblr (WX, FF, CH, CU)
* raptor-tp6-twitch (WX, FF, CH, CU)
* raptor-tp6-twitter (WX, FF, CH, CU)
* raptor-tp6-wikipedia (WX, FF, CH, CU)
* raptor-tp6-yahoo-mail (WX, FF, CH, CU)
* raptor-tp6-yahoo-news (WX, FF, CH, CU)
* raptor-tp6-yandex (WX, FF, CH, CU)
* raptor-tp6-youtube (WX, FF, CH, CU)
* wikipedia (BT)
* yahoo-news (BT)
* youtube (BT)

Live
----
A set of test pages that are run as live sites instead of recorded versions. These tests are available on all browsers, on all platforms.


Mobile
------
Page-load performance test suite on Android. (WX: WebExtension, BT: Browsertime, GV: Geckoview, RB: Refbrow, FE: Fenix, F68: Fennec68)

* allrecipes (BT)
* amazon (BT)
* amazon-search (BT)
* bbc (BT)
* bing (BT)
* bing-search-restaurants (BT)
* booking (BT)
* cnn (BT)
* cnn-ampstories (BT)
* ebay-kleinanzeigen (BT)
* ebay-kleinanzeigen-search (BT)
* espn (BT)
* facebook (BT)
* facebook-cristiano (BT)
* google (BT)
* google-maps (BT)
* google-search-restaurants (BT)
* imdb (BT)
* instagram (BT)
* jianshu (BT)
* microsoft-support (BT)
* raptor-tp6m-allrecipes (WX, GV, RB, FE, F68)
* raptor-tp6m-amazon (WX, GV, RB, FE, F68)
* raptor-tp6m-amazon-search (WX, GV, RB, FE, F68)
* raptor-tp6m-bbc (WX, GV, RB, FE, F68)
* raptor-tp6m-bing (WX, GV, RB, FE, F68)
* raptor-tp6m-bing-restaurants (WX, GV, RB, FE, F68)
* raptor-tp6m-booking (WX, GV, RB, FE, F68)
* raptor-tp6m-cnn (WX, GV, RB, FE, F68)
* raptor-tp6m-cnn-ampstories (WX, GV, RB, FE, F68)
* raptor-tp6m-ebay-kleinanzeigen (WX, GV, RB, FE, F68)
* raptor-tp6m-espn (WX, GV, RB, FE, F68)
* raptor-tp6m-facebook (WX, GV, RB, FE, F68)
* raptor-tp6m-facebook-cristiano (WX, GV, RB, FE, F68)
* raptor-tp6m-google (WX, GV, RB, FE, F68)
* raptor-tp6m-google-maps (WX, GV, RB, FE, F68)
* raptor-tp6m-google-restaurants (WX, GV, RB, FE, F68)
* raptor-tp6m-imdb (WX, GV, RB, FE, F68)
* raptor-tp6m-instagram (WX, GV, RB, FE, F68)
* raptor-tp6m-jianshu (WX, GV, RB, FE, F68)
* raptor-tp6m-microsoft-support (WX, GV, RB, FE, F68)
* raptor-tp6m-reddit (WX, GV, RB, FE, F68)
* raptor-tp6m-stackoverflow (WX, GV, RB, FE, F68)
* raptor-tp6m-web-de (WX, GV, RB, FE, F68)
* raptor-tp6m-wikipedia (WX, GV, RB, FE, F68)
* raptor-tp6m-youtube (WX, GV, RB, FE, F68)
* raptor-tp6m-youtube-watch (WX, GV, RB, FE, F68)
* reddit (BT)
* stackoverflow (BT)
* web-de (BT)
* wikipedia (BT)
* youtube (BT)
* youtube-watch (BT)

Scenario
--------
Tests that perform a specific action (a scenario), i.e. idle application, idle application in background, etc.


Unittests
---------
These tests aren't used in standard testing, they are only used in the Raptor unit tests (they are similar to raptor-tp6 tests though).



The methods for calling the tests can be found in the `Raptor wiki page <https://wiki.mozilla.org/TestEngineering/Performance/Raptor>`_.
