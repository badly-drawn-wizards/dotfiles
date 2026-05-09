/* Wrapper that prepends RELAX_LIB to LD_PRELOAD before exec'ing the
 * real azurevpnclient-unprivileged. The relax .so's constructor then
 * runs in the main binary and every subprocess MSAL spawns, doing the
 * cap-drop / dumpable work needed for xdg-desktop-portal to identify
 * callers (see azurevpnclient-relax.c).
 *
 * LD_PRELOAD is honored across the exec chain because no privilege
 * boundary is crossed (caps come from ambient, file caps stay zero on
 * targets after the initial /run/wrappers exec, so AT_SECURE=0).
 *
 * WRAPPED_TARGET and RELAX_LIB are injected at compile time.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef WRAPPED_TARGET
#error "WRAPPED_TARGET must be defined at compile time"
#endif
#ifndef RELAX_LIB
#error "RELAX_LIB must be defined at compile time"
#endif

int main(int argc, char *argv[]) {
    const char *existing = getenv("LD_PRELOAD");
    char preload[4096];
    if (existing && *existing) {
        snprintf(preload, sizeof(preload), "%s:%s", RELAX_LIB, existing);
    } else {
        snprintf(preload, sizeof(preload), "%s", RELAX_LIB);
    }
    setenv("LD_PRELOAD", preload, 1);

    execv(WRAPPED_TARGET, argv);
    perror("execv");
    return 1;
}
