{
  profiles."lly2q038.default" = {
    userChrome = ''
      /* Hide tab bar in FF Quantum */
      @-moz-document url("chrome://browser/content/browser.xul") {
        #TabsToolbar {
          visibility: collapse !important;
          margin-bottom: 21px !important;
        }
        #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
          visibility: collapse !important;
        }
      }
    '';
  };
}
