let
  unstable = import <nixos-unstable> {};
in

self: super:
{
  
  iosevka-ss02-bin = unstable.iosevka-bin.override {
    variant = "ss02";
  };

  polybar = super.polybar.override {
    pulseSupport = true;
  };
}
