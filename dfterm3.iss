[Setup]
AppName=Dfterm3
AppVersion=0.4
DefaultDirName={pf}\Dfterm3
DefaultGroupName=Dfterm3
LicenseFile=LICENSE
VersionInfoVersion=0.4

[Icons]
Name: "{group}\Dfterm3 server"; Filename: "{app}\dfterm3.exe"; WorkingDir: "{app}"
Name: "{group}\Uninstall Dfterm3"; Filename: "{uninstallexe}"
Name: "{commondesktop}\Dfterm3 server"; Filename: "{app}\dfterm3.exe"; WorkingDir: "{app}"

[Files]
Source: "dist\build\Dfterm3\dfterm3.exe"; DestDir: "{app}"; Flags: replacesameversion
Source: "web-interface\admin-login.html"; DestDir: "{app}\web-interface"
Source: "web-interface\playing.html"; DestDir: "{app}\web-interface"
Source: "web-interface\resources\cootue_curses_square_16x16.ttf"; DestDir: "{app}\web-interface\resources"
Source: "web-interface\resources\dfterm3_term.css"; DestDir: "{app}\web-interface\resources"
Source: "web-interface\resources\interface.css"; DestDir: "{app}\web-interface\resources"
Source: "web-interface\resources\interface.js"; DestDir: "{app}\web-interface\resources"
Source: "web-interface\js\playing.js"; DestDir: "{app}\web-interface\js"
Source: "web-interface\js\terminal.js"; DestDir: "{app}\web-interface\js"
Source: "web-interface\js\timing.js"; DestDir: "{app}\web-interface\js"

[Run]
Filename: "{app}\dfterm3.exe"; Description: "Launch Dfterm3"; Flags: postinstall nowait skipifsilent unchecked
