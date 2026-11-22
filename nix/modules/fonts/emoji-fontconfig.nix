# emoji-fontconfig --- the referee between noto and symbola
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
    # instead of color glyphs. slot the color font into each generic family
    # right after the text font, so emoji beat symbola while symbola stays
    # available for the unicode coverage emacs relies on.
    #
    # the text font has to come first: noto color emoji carries glyphs for
    # 0-9, #, * and space (for keycaps), so putting it at the front makes
    # every page draw its digits and spaces from the emoji font.
    xdg.configFile."fontconfig/conf.d/75-emoji-color.conf".text = ''
      <?xml version="1.0"?>
      <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
      <fontconfig>
        <alias binding="strong">
          <family>sans-serif</family>
          <prefer>
            <family>Noto Sans</family>
            <family>Noto Color Emoji</family>
          </prefer>
        </alias>
        <alias binding="strong">
          <family>serif</family>
          <prefer>
            <family>Noto Serif</family>
            <family>Noto Color Emoji</family>
          </prefer>
        </alias>
        <alias binding="strong">
          <family>monospace</family>
          <prefer>
            <family>Noto Sans Mono</family>
            <family>Noto Color Emoji</family>
          </prefer>
        </alias>
        <alias binding="strong">
          <family>emoji</family>
          <prefer><family>Noto Color Emoji</family></prefer>
        </alias>
      </fontconfig>
    '';
  };
}
