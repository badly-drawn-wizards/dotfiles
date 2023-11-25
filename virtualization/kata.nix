{ config, lib, pkgs, ... }:
let
  settingsFormat = pkgs.formats.toml { };
  cfg = config.virtualisation.kata-containers;
  configFile = settingsFormat.generate "configuration.toml" cfg.settings;
in
{
  options = {
    virtualisation.kata-containers.settings = lib.mkOption {
      type = settingsFormat.type;
      default = builtins.fromTOML ./kata-configuration-qemu.toml;
      description = ''
        Settings for kata's configuration.toml
      '';
    };
  };

  config =
    let
      var-kata-images = "/var/lib/kata-containers";
    in
    {
      virtualisation.kata-containers.settings = builtins.fromTOML (builtins.readFile ./kata-configuration-qemu.toml) // {
        hypervisor.qemu = {
          path = "${pkgs.qemu_kvm}/bin/qemu-system-x86_64";
          kernel = "${var-kata-images}/vmlinux.container";
          image = "${var-kata-images}/kata-containers.img";
          # initrd = "${var-kata-images}/kata-containers-initrd.img";

          valid_hypervisor_paths = [ "${pkgs.qemu_kvm}/bin/qemu-system-x86_64" ];

          virtio_fs_daemon = "${pkgs.virtiofsd}/bin/virtiofsd";
          virtio_fs_daemon_paths = [ "${pkgs.virtiofsd}/bin/virtiofsd" ];
          machine_type = "q35";
          rootfs_type = "ext4";
          disable_selinux = true;
          disable_guest_selinux = true;
        };
        agent.kata = {
          enable_debug = true;
          enable_tracing = true;
          debug_console_enabled = true;
        };
        runtime = {
          enable_debug = true;
          enable_tracing = true;
          disable_guest_seccomp = true;
        };
      };

      environment.etc."kata-containers/configuration.toml".source = configFile;

      systemd.services.containerd = {
        serviceConfig.ExecStartPre = [ "${pkgs.coreutils}/bin/cp -a ${pkgs.kata-images}/share/kata-containers/. /var/lib/kata-containers/" ];
        path = [ pkgs.kata-runtime ];
      };

      virtualisation.containerd.settings = {
        debug.level = "debug";
        plugins = {
          "io.containerd.grpc.v1.cri".containerd = {
            untrusted_workload_runtime = {
              runtime_type = "io.containerd.kata-qemu.v2";
            };
            runtimes.kata-qemu = {
              runtime_type = "io.containerd.kata-qemu.v2";
              privileged_without_host_devices = true;
              pod_annotations = [ "io.katacontainers.*" ];
              options = {
                BinaryName = "${pkgs.kata-runtime}/bin/containerd-shim-kata-qemu-v2 ";
                SystemdCgroup = true;
              };
            };
            runtimes.runc.runtime_type = "io.containerd.runc.v2";
          };
        };
      };
    };
}
