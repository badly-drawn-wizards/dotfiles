/* LD_PRELOAD constructor that runs in azurevpnclient and every
 * subprocess it spawns. Two jobs:
 *
 *   1. In the trusted VPN-binary chain (shim, base binary wrapper,
 *      real binary): set PR_SET_DUMPABLE,1. The kernel resets it to 0 on
 *      the cap-elevating exec from /run/wrappers/bin/azurevpnclient,
 *      which makes /proc/[PID] root-owned and unreadable by sibling
 *      xdg-desktop-portal. Children inherit dumpable across non-cap-
 *      elevating execs, so setting it once in the chain is enough.
 *
 *   2. In every other subprocess (the gdbus child that calls
 *      org.freedesktop.portal.OpenURI for AAD browser launch,
 *      xdg-open, dbus-send, etc.): drop all caps including ambient.
 *      The kernel's __ptrace_may_access requires the caller's
 *      permitted set to be a superset of the target's, independent of
 *      UID and dumpable. Portal has no caps, so any subprocess that
 *      inherited CAP_NET_ADMIN via ambient is unreachable to it.
 *      The main binary keeps caps to create the tun device.
 */

#define _GNU_SOURCE
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <linux/capability.h>

#ifndef PR_CAP_AMBIENT
#define PR_CAP_AMBIENT 47
#endif
#ifndef PR_CAP_AMBIENT_CLEAR_ALL
#define PR_CAP_AMBIENT_CLEAR_ALL 4
#endif

static int is_trusted(const char *path) {
    if (!path) return 0;
    const char *base = strrchr(path, '/');
    base = base ? base + 1 : path;
    return strcmp(base, "microsoft-azurevpnclient") == 0
        || strcmp(base, "azurevpnclient-unprivileged") == 0
        || strcmp(base, "azurevpnclient") == 0;
}

__attribute__((constructor))
static void azurevpn_relax(void) {
    if (is_trusted(program_invocation_name)) {
        prctl(PR_SET_DUMPABLE, 1, 0, 0, 0);
        return;
    }

    prctl(PR_CAP_AMBIENT, PR_CAP_AMBIENT_CLEAR_ALL, 0, 0, 0);
    struct __user_cap_header_struct hdr = {
        .version = _LINUX_CAPABILITY_VERSION_3,
        .pid = 0,
    };
    struct __user_cap_data_struct data[2] = { {0}, {0} };
    syscall(SYS_capset, &hdr, data);
}
