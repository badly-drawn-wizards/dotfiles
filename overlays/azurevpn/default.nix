self: super:

# Make microsoft-azurevpnclient's interactive AAD auth work on NixOS, and
# patch the AAD audience for compatibility with VPN gateways that still
# use the legacy (Windows-only) audience.
#
# The kernel's __ptrace_may_access has a capability-subset rule: a sibling
# process can only inspect /proc/$PID/* of a target whose permitted-cap
# set is a subset of its own. NixOS's security.wrappers raises
# CAP_NET_ADMIN via ambient, which the main binary needs for tun
# creation — but the cap also propagates to every subprocess via
# ambient, including the gdbus helper that calls
# org.freedesktop.portal.OpenURI to launch the AAD browser. Portal (no
# caps) can't readlink /proc/$gdbus/root and refuses with AccessDenied,
# MSAL gets exit-4, auth fails.
#
# Cap fix: an LD_PRELOAD .so whose constructor drops all caps in every
# subprocess that isn't part of the trusted VPN-binary chain.
#
# Audience fix: byte-swap the modern AAD audience GUID baked into
# libLinuxCore.so to the legacy GUID, applied via postFixup on the
# upstream derivation so the makeBinaryWrapper paths embedded in bin/*
# wrappers point at the patched lib's $out (no path rewriting needed).
#
# Chain:
#   /run/wrappers/bin/azurevpnclient            [fcaps wrapper, raises ambient]
#   → ${shim}/bin/azurevpnclient-unprivileged   [sets LD_PRELOAD=relax.so]
#   → ${base}/bin/azurevpnclient-unprivileged   [makeBinaryWrapper, paths -> patched ${base}]
#   → ${base}/.../microsoft-azurevpnclient      [real binary, loads patched libLinuxCore.so]
#   → MSAL helpers / gdbus / xdg-open           [relax.so drops caps so portal can identify]

let
  modernAudience = "c632b3df-fb67-4d84-bdcf-b95ad541b5c8";
  legacyAudience = "41b23e61-6c1e-4545-b367-cd054e0ed4b4";

  base = super.microsoft-azurevpnclient.overrideAttrs (old: {
    postFixup = (old.postFixup or "") + ''
      ${super.python3}/bin/python3 -c "
      p = '$out/opt/microsoft/microsoft-azurevpnclient/lib/libLinuxCore.so'
      old = b'${modernAudience}'
      new = b'${legacyAudience}'
      assert len(old) == len(new)
      data = open(p, 'rb').read()
      n = data.count(old)
      assert n == 1, f'expected 1 occurrence of audience, found {n}'
      open(p, 'wb').write(data.replace(old, new))
      print(f'patched {n} audience occurrence(s) in libLinuxCore.so')
      "
    '';
  });

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
    # Order matters: shim's bin/azurevpnclient-unprivileged must shadow
    # base's. lndir-style joining keeps the first-seen entry.
    paths = [ shim base ];
    inherit (base) meta version;
    pname = "${base.pname or "microsoft-azurevpnclient"}-with-dumpable-shim";
  };
}
