self: super: {
  microsoft-azurevpnclient = super.microsoft-azurevpnclient.overrideAttrs (old: {
    postFixup = (old.postFixup or "") + ''
      sed -i 's/c632b3df-fb67-4d84-bdcf-b95ad541b5c8/41b23e61-6c1e-4545-b367-cd054e0ed4b4/g' \
        $out/opt/microsoft/microsoft-azurevpnclient/lib/libLinuxCore.so
    '';
  });
}
