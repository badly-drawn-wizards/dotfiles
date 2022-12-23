self: super:

let
  pkgs = self;
  inherit (pkgs) lib recurseIntoAttrs;
  structuredExtraConfig = with lib.kernel; {
    ACPI_DEBUG = yes;
    ACPI_DEBUGGER = yes;
    ACPI_CUSTOM_METHOD = yes;
    DEBUG_FS = yes;
    ACPI_DEBUGGER_USER = module;
    DEBUG_INFO = yes;
  };
in
{
  linuxKernel = super.linuxKernel // {
    kernels = super.linuxKernel.kernels.extend (kself: ksuper: {
      linux_custom = self.linuxPackages_latest.kernel.override ({
        inherit structuredExtraConfig;
        # argsOverride = {
        #   src = pkgs.linuxSrc_custom;
        #   separateDebugInfo = true;
        # };
      });
      linux_ccache = kself.linux_custom.override {
        stdenv = pkgs.ccacheStdenv;
        buildPackages = pkgs.buildPackages // {
          stdenv = pkgs.ccacheStdenv;
        };
      };
    });
    vanillaPackages = super.linuxKernel.vanillaPackages // {
      linux_custom =
        recurseIntoAttrs (
          self.linuxKernel.packagesFor self.linuxKernel.kernels.linux_custom
        );
      linux_ccache =
        recurseIntoAttrs (
          self.linuxKernel.packagesFor self.linuxKernel.kernels.linux_ccache
        );
    };
    packageAliases = super.linuxKernel.packageAliases // {
      linux_default = self.linuxKernel.packages.linux_custom;
    };
  };
}
