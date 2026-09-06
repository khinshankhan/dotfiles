{ lib, config, ... }:

let
  cfg = config.modules.fonts.emoji-fontconfig;
in {
  options.modules.fonts.emoji-fontconfig = {
    enable = lib.mkEnableOption "prefer color emoji over monochrome fallbacks";
  };

  config = lib.mkIf cfg.enable {
    # symbola covers the emoji codepoints too, but monochrome. without this
    # rule fontconfig may resolve emoji to symbola and render flat outlines
    # instead of color glyphs. prepend the color font to each generic family
    # so emoji win there, while symbola stays available for the unicode
    # coverage emacs relies on.
    xdg.configFile."fontconfig/conf.d/75-emoji-color.conf".text = ''
      <?xml version="1.0"?>
      <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
      <fontconfig>
        <alias binding="strong">
          <family>sans-serif</family>
          <prefer><family>Noto Color Emoji</family></prefer>
        </alias>
        <alias binding="strong">
          <family>serif</family>
          <prefer><family>Noto Color Emoji</family></prefer>
        </alias>
        <alias binding="strong">
          <family>monospace</family>
          <prefer><family>Noto Color Emoji</family></prefer>
        </alias>
        <alias binding="strong">
          <family>emoji</family>
          <prefer><family>Noto Color Emoji</family></prefer>
        </alias>
      </fontconfig>
    '';
  };
}
