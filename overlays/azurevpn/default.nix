self: super:

# Make microsoft-azurevpnclient's interactive AAD auth work on NixOS.
#
# The kernel's __ptrace_may_access has a capability-subset rule:
# a sibling process can only inspect /proc/$PID/* of a target whose
# permitted-cap set is a subset of its own. NixOS's security.wrappers
# raises CAP_NET_ADMIN via ambient, which the main binary needs for
# tun creation — but the cap also propagates to every subprocess via
# ambient, including the gdbus helper that calls
# org.freedesktop.portal.OpenURI to launch the AAD browser. Portal
# (no caps) can't readlink /proc/$gdbus/root and refuses with
# AccessDenied, MSAL gets exit-4, auth fails.
#
# Fix: an LD_PRELOAD .so whose constructor drops all caps in every
# subprocess that isn't part of the trusted VPN-binary chain. The
# constructor also re-applies PR_SET_DUMPABLE,1 in the main binary
# (kernel resets it on the cap-elevating exec from /run/wrappers/bin).
#
# Chain:
#   /run/wrappers/bin/azurevpnclient            [fcaps wrapper, raises ambient]
#   → ${shim}/bin/azurevpnclient-unprivileged   [sets LD_PRELOAD=relax.so]
#   → ${base}/bin/azurevpnclient-unprivileged   [makeBinaryWrapper / wrapGApp env-setter]
#   → ${base}/.../microsoft-azurevpnclient      [real binary]
#   → MSAL helpers / gdbus / xdg-open           [relax.so drops caps so portal can identify]

let
  base = super.microsoft-azurevpnclient;

  relax = super.runCommandCC "azurevpnclient-relax" { } ''
    mkdir -p $out/lib
    cc -O2 -Wall -shared -fPIC -nostartfiles \
      -o $out/lib/libazurevpnclient-relax.so \
      ${./azurevpnclient-relax.c}
  '';

  shim = super.runCommandCC "azurevpnclient-dumpable-shim" { } ''
    mkdir -p $out/bin
    cc -O2 -Wall \
      "-DWRAPPED_TARGET=\"${base}/bin/azurevpnclient-unprivileged\"" \
      "-DRELAX_LIB=\"${relax}/lib/libazurevpnclient-relax.so\"" \
      -o $out/bin/azurevpnclient-unprivileged \
      ${./azurevpnclient-dumpable-shim.c}
  '';
in
{
  microsoft-azurevpnclient = super.symlinkJoin {
    name = "microsoft-azurevpnclient-with-dumpable-shim";
    # Order matters: shim's bin/azurevpnclient-unprivileged must
    # shadow base's. lndir-style joining keeps the first-seen entry.
    paths = [ shim base ];
    inherit (base) meta version;
    pname = "${base.pname or "microsoft-azurevpnclient"}-with-dumpable-shim";
  };
}
