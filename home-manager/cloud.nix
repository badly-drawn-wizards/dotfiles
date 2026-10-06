{ config, lib, pkgs, ... }:

{
  # Cloud and infrastructure CLIs.
  #
  # Tools belong here rather than in dotfiles-private — that repo carries secrets
  # and sensitive detail only. az previously lived in the MasscloudAPI devenv,
  # which meant it vanished outside that directory.
  #
  # Extensions must be baked in: `az extension add` writes to ~/.azure, outside
  # the closure, so it would drift away on the next rebuild.
  home.packages = [
    (pkgs.azure-cli.withExtensions (with pkgs.azure-cli-extensions; [
      azure-devops  # az repos / az pipelines — the Azure DevOps repos the work lives in
      log-analytics # az monitor log-analytics — KQL against the recon log workspaces
    ]))
    pkgs.scalr-cli # Scalr API — the OpenTofu remote backend for Masscloud infrastructure
  ];
}
