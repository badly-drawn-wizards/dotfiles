# The NodeName=/PartitionName= renderer.
#
# Both the controller fragment and the module's `nodeEntry`/`partitionEntries`
# read-only options go through this file, so the line a node advertises and the
# line the controller writes are produced by the same code. The formats below
# reproduce the controller's existing slurm.conf byte for byte (including the
# two spaces that align the non-default PartitionName lines).
{ lib, inventory }:
let
  optional = name: value: lib.optional (value != null) "${name}=${toString value}";

  partitionMembers = p: builtins.filter (n: builtins.elem p.name n.partition) inventory.nodes;
  partitionMemberNames = p: map (n: n.name) (partitionMembers p);

  renderNode =
    n:
    lib.concatStringsSep " " (
      [ n.name ]
      ++ optional "NodeAddr" n.addr
      ++ optional "Port" n.port
      ++ optional "NodeHostname" n.nodeHostname
      ++ [ "CPUs=${toString n.cpus}" ]
      ++ lib.optionals (n.topology != null) [
        "Sockets=${toString n.topology.sockets}"
        "CoresPerSocket=${toString n.topology.coresPerSocket}"
        "ThreadsPerCore=${toString n.topology.threadsPerCore}"
      ]
      ++ [ "RealMemory=${toString n.realMemoryMb}" ]
      ++ lib.optional (n.features != [ ]) "Features=${lib.concatStringsSep "," n.features}"
      ++ optional "Weight" n.weight
      ++ [ "State=${n.state}" ]
    );

  renderPartition =
    p:
    let
      nodes = lib.concatStringsSep "," (partitionMemberNames p);
    in
    if p.default then
      "${p.name} Nodes=${nodes} Default=YES MaxTime=${p.maxTime} State=${p.state}"
    else
      "${p.name}  Nodes=${nodes}  MaxTime=${p.maxTime} State=${p.state}";
in
{
  inherit renderNode renderPartition partitionMembers partitionMemberNames;
}
