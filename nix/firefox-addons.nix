{pkgs, ...}: with pkgs.nur.repos.rycee.firefox-addons; {
  hide-scrollbars = buildFirefoxXpiAddon {
    pname = "hide-scrollbars";
    version = "3.0.1";
    addonId = "{a250ed19-05b9-4486-b2c3-535044766b8c}";
    url = "https://addons.mozilla.org/firefox/downloads/file/3760332/hide_scrollbars-4.0.2.xpi";
    sha256 = "DT0ASp8B+dZhoqQlkMjiGxHceUHmaNykzUk6hUi1AS0=";
    meta.description = "Hide scrollbars on Firefox v57+";
  };

  tab-center-reborn = buildFirefoxXpiAddon {
    pname = "tab-center-reborn";
    version = "2.3.1";
    addonId = "tabcenter-reborn@ariasuni";
    url = "https://addons.mozilla.org/firefox/downloads/file/3829515/tabcenter_reborn-2.3.1.xpi";
    sha256 = "0xxpPIlgRdQybH6eAVKDCCAAm9YPYrNgQ7syLKtxPzQ=";
    meta.description = "Simple and powerful vertical tab bar.";
  };
}
