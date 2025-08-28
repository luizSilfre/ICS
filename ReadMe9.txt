ICS - Internet Component Suite - V9.4 - Delphi 7 to RAD Studio 12
=================================================================
(Aka FPIETTE's Components)


Revised: February 12, 2025
Release: V9.4
https://www.overbyte.eu/
https://wiki.overbyte.eu/
https://svn.overbyte.be/svn/

ICS is a free internet component library for all Delphi, C++Builder, BDS and RAD Studio versions.
It includes TCP, UDP, raw sockets, clients, servers, as well as all the main high level protocols
such as FTP, SMTP, POP3, NNTP, HTTP and more. ICS also supports SSL/TLS with the help of OpenSSL.

Includes OpenSSL 3.0.16, 3.2.4, 3.3.3 and 3.4.1 for Win32 and Win64.


UPGRADE TO V9.1 AND LATER WARNINGS
----------------------------------

https://wiki.overbyte.eu/wiki/index.php/Updating_projects_to_V9.1

There are a lot of changes between ICS V8/V9.0, and ICS V9.1 and later, and some
less major changes with ICS V9.3 that will require OverbyteIcsTypes being added to
most applications.

Opening project forms with the IDE should automatically add any new units required,
but OverbyteIcsTypes and OverbyteIcsSslBase may need adding manually if ICS components
are created in code.

Installation for Delphi 10.4 and later all now use the same install groups and projects,
and may need the library path updating for versions, you must uninstall earlier packages
before installing new packages, due to the package names having changed.

The default location for the OpenSSL DLLs has moved.

There are several new DEFINES relating to building SSL/TLS applications that need adding to
the OverbyteIcsDefs.inc file, if it is not replaced during installation.

All the samples are in new directories, many old samples have been archived.

More details below.


Table of content:
-----------------

- Legal issues
- Donate
- Register
- Contributions
- Support
- Latest Versions
- Version Control repository
- Platforms and Targets
- Installation
- SSL/TLS Optional DEFINES
- 'ICS Root CA' Certificate
- SSL/TLS Updating to V9.1 and later
- SSL/TLS Downloads
- Available VCL Components
- Delphi Windows sample applications:
- Getting Started with ICS
- Release notes
- Midware


Legal issues:
-------------
              Copyright (C) 1997-2025 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium
              <francois.piette@overbyte.be>

              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany

              ICS is freeware.

              This software is provided 'as-is', without any express or
              implied warranty. In no event will the author be held liable
              for any damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

              5. As this code make use of OpenSSL, your rights are restricted
                 by OpenSSL license as soon as you use any SSL feature.
                 See http://www.openssl.org for details.


Donate
------

ICS is freeware. You can use it without paying anything except the registration
postcard (see "register" below). But of course donations are welcome. You can
send cash (Euro currency or US Dollars) in an envelop to my street address or
buy a gift certificate at Amazon in the UK. I will then use it to buy books.
Here is the direct URL at Amazon UK (nearest to my home, please don't use another):
http://www.amazon.co.uk/exec/obidos/gc-email-order1/ref=g_gc_email/202-6198323-6681414
For more generous amount, contact me by email.


Register
--------

ICS is freeware. If you use the components, you must register by sending a
picture postcard showing the area you live in and some beautiful stamps for
my kids who are stamp collectors. Do not use an envelop, I collect USED
postcards sent to me. Write on the postcard that it is your ICS registration.

Address your card to: Francois PIETTE, rue de Grady 24, 4053 Embourg, Belgium.
Don't forget to mention your name, street address, EMail and web site.


Contributions:
--------------

ICS has been designed by François PIETTE but many other peoples are working on the
components and sample programs. The history of changes in each source file list
all developers having contributed (When no name is given, the change is by F. Piette).
I can't list all contributors here but I want to specially thanks two specially active
contributors:
    - Arno Garrels
    - Angus Robertson <angus@magsys.co.uk>


Support:
--------

A new web support forum was created for ICS in February 2019:

https://en.delphipraxis.net/forum/37-ics-internet-component-suite/

Once registered, it is possible to follow a forum with email messages for new
posts, or a daily summary like the old mailing list.


Latest versions:
---------------

The latest versions of ICS can be downloaded from the ICS Wiki web site:

https://wiki.overbyte.eu/wiki/index.php/ICS_Download

ICS V5, V6, V7 and V8 are archive releases no longer updated or supported.

ICS V9 is the long term support release which is held in a public Version Control
repository that is zipped each night for easy download.  The download page above
also includes the OpenSSL binaries needed to support SSL. ICS V9 supports Delphi
64-bit and Mac OS-X projects.  Note that C++ Builder versions supported are 10.4
and later.  Beware Mac OS-X and C++ have not been tested recently due to lack of
support from such users.

The latest released version is V9.4 which will be reported by the CopyRight constant
in OverbyteIcsWSocket.pas and the integer WSocketVersion as 904.

ICS V10 is in early development and is planned to support Android and Linux Server.
There are no current plans for ICS for iOS or MacOS.


Version Control repository:
---------------------------

svn://svn.overbyte.be/ or https://svn.overbyte.be/svn/
There are several repositores, icsv9 is the latest release, icsdev contains some
older samples and other ICS tools, ddservice is needed for two ICS Windows service
samples.
(Usercode = ics, password = ics)


Platforms and Targets
---------------------

The latest version of ICS supports Windows 32-bit and 64-bit targets using VCL and FMX
components, and Apple MacOS 32-bit and 64-bit platforms with FMX (Delphi permitting).

Testing of ICS is primarily with Windows 11 and Server 2019 and 2022, also Windows 10.
Note that Windows XP is not supported and SSL will not work. Windows 7, 8 and Server
2008 R2 and Server 2012 are still be supported but testing is minimal.


Installation:
-------------

ICS V9 has been designed for Embarcadero Delphi 2009 and up, and C++ Builder
2009 and up, but is mostly compatible with Borland Delphi 7 and CodeGear 2006 and
2007. Delphi 7 does not have TWebBrowser so the TOAuthLoginForm unit is missing
and samples needing it will fail.

While ICS still includes packages for Delphi 7 and it should still work, we can no
longer regularly test changes against Delphi 7 so you may find incompatibilities
with ICS due to it's old syntax and libraries, please submit any corrections that
are needed for future releases. Many samples may fail to built without changes with
older compilers, too many changes over the years.

Embarcadero RAD Studio includes Delphi and C++ Builder.

https://www.embarcadero.com/

With Delphi XE2 and later, VCL 64-bit Windows targets are supported for Delphi only.
Currently FireMonkey is partly supported for Delphi only (there are still a few
non-ported components). ICS for Mac OSX is currently experimental.

When extracting ICS from the zip file, the directory structure MUST be maintained,
otherwise you will be unable to use the install groups and library package projects
to correctly build ICS.

This is the V9.1 and later sub-directory layout:

.\                                    Info directory
.\Install                             Component packages project groups for all versions
.\Packages                            Delphi (7 and up) and C++Builder (2006 and up) packages projects
.\Source                              ICS Delphi source code built into packages
.\Source\Include                      .inc files (including OverbyteIcsDefs.inc)
.\Source\zobj1212                     ZLIB C OBJ include files
.\ICS-OpenSSL                         OpenSSL DLLs, copied to c:\ProgramData\ICS-OpenSSL\ when
                                        building packages, including sub-directories.
.\ICS-OpenSSL\ICS-Certs               ICS SSL/TLS test certificates
.\ICS-OpenSSL\ICS-RootCAs             ICS SSL/TLS root certificate authority bundles
.\Lib\$(Config)\$(Platform)\$(ProductVersion)
                                      Unit output directories for all package builds,
                                        subdirectories created on building the packages, Release/
                                        Debug, Win32/Win64/OSX64, D2007/D110//etc/21.0/22.0/23.0,
                                        includes .dcu and .dfm files for Delphi and .obj and .hpp
                                        files for C++ Builder
.\demos-delphi-vcl                    VCL samples for Windows
.\demos-delphi-extra                  VCL samples that need third party components to build
.\demos-delphi-fmx                    FMX samples for Windows, not yet tested on MacOS
.\demos-delphi-mobile                 Empty, for the future
.\demos-cpp-vcl                       Old C++ samples that have not been tested for 10 years
.\demos-data                          Data files for samples, such as web pages

The .\ indicates the directory into which you extracted the ICS archive, your choice,
but avoid c:\program files due to file permissions.
Example directories could be c:\icsv9, c:\delpicomp\icsv9, etc.

Note the main change from ICS V9.0 and earlier is the Samples directory has been re-arranged
to make it easier to find useful samples in more sensibly named directories.

There are major directory and file changes between ICS V9.0 and earlier, and ICS V9.1
and later, hundreds of old files have been removed, and many files are in new directories.
It is strongly recommended the old install directory is renamed old or something, and
V9.1 or later is extracted to a clean directory, to avoid having a mix of old and new files.


UPGRADING and REINSTALLING

Uninstall an existing ICS package (Menu | Component | Install Packages, select the component
package and click Remove).  If you skip uninstall for Delphi 10.4 and later updating from ICS
V9.0 or earlier, you may get errors due to the package names having changed.

Rename the old ICS directory and unzip to a new or empty directory, remove the
old path from the library path and add either the new .\Source directory to the library
path under Tools | Options |... or the appropriate .\Lib subdirectory according to
version, ie .\Lib\Debug\Win32\D2007 for Delphi 2007.

The latter has the advantage that the ICS source code won't be recompiled whenever
your project is build. Also under Tools | Options |... add the new .\Source directory
to the Browsing path.


All DELPHI and C++ BUILDER VERSIONS

Always upgrade your compiler with the latest update available from Embarcadero.
Always update your system with https://windowsupdate.microsoft.com


INSTALLATION USING THE INSTALL PROJECT GROUPS

ICS provides three different install groups for compilers supporting both VCL and FMX,
to allow either alone to be installed, or both installed together.

For Delphi 10.4, 11, 12 and later, the same install groups and packages are used for
all Delphi and C++ versions since these the support $(Auto) library suffix which causes
packages to be build with the compiler version instead of the manually entered Delphi
version needed for earlier compilers, thus requiring compiler specific packages.

Project group are provided in directory .\Install, and refer to projects in .\Packages,
and source files in .\Source, they will not build for a different directory structure.

Delphi 10.4/11/12       : IcsInstallVcl.groupproj       // VCL components for Windows
Delphi 10.4/11/12       : IcsInstallFmx.groupproj       // FMX components for Windows
Delphi 10.4/11/12       : IcsInstallVclFmx.groupproj    // VCL and FMX components for Windows
Delphi 10.4/11/12       : IcsInstallTestPosix.groupproj // FMX test components for MacOS/Android/Linux
C++ Builder 10.4/11/12  : CBIcsInstallVclFmx.groupproj  // VCL and FMX components for Windows

WARNING - only Delphi 10.41 and 10.42 (10.4 with updates 1 or 2) will install correctly with
the above packages, the original RTM version does not support the package LIB suffix: $(Auto)
so you must change it manually for each package to 21.0.

C++ Builder Warning - due to lack of testing by ICS C++ users, these packages may not install
correctly.  If you need ICS C++ support, you will need to fix any problems yourself.

If you are updating Delphi 10.41 or later from ICS V9.0 or earlier, the old packages with
a version number must be removed manually before installing the new packages, or running the
application 'icsremoldpacks.exe -DALL' will remove the old pacakges from 10.41 and later.

For compilers before Delphi 10.4, there are version specific groups:

Delphi 7         :  D7Install.bpg               // VCL components for Windows
Delphi 2006      :  D2006Install.bdsgroup       // VCL components for Windows
Delphi 2007      :  D2007Install.groupproj      // VCL components for Windows
Delphi 2009      :  D2009Install.groupproj      // VCL components for Windows
Delphi 2010      :  D2010Install.groupproj      // VCL components for Windows
Delphi XE        :  DXeInstall.groupproj        // VCL components for Windows

Delphi XE2       :  DXe2InstallVcl.groupproj    // VCL components for Windows
Delphi XE2       :  DXe2InstallFmx.groupproj    // FMX components for Windows
Delphi XE2       :  DXe2InstallVclFmx.groupproj // VCL and FMX components for Windows

Delphi XE3       :  DXe3InstallVcl.groupproj    // VCL components for Windows
Delphi XE3       :  DXe3InstallVclFmx.groupproj // FMX components for Windows
Delphi XE3       :  DXe3InstallVclFmx.groupproj // VCL and FMX components for Windows

Delphi XE4       :  DXe4InstallVcl.groupproj    // VCL components for Windows
Delphi XE4       :  DXe4InstallFmx.groupproj    // FMX components for Windows
Delphi XE4       :  DXe4InstallVclFmx.groupproj // VCL and FMX components for Windows

Delphi XE5       :  DXe5InstallVcl.groupproj    // VCL components for Windows
Delphi XE5       :  DXe5InstallFmx.groupproj    // FMX components for Windows
Delphi XE5       :  DXe5InstallVclFmx.groupproj // VCL and FMX components for Windows

Delphi XE6       :  DXe6InstallVcl.groupproj    // VCL components for Windows
Delphi XE6       :  DXe6InstallFmx.groupproj    // FMX components for Windows
Delphi XE6       :  DXe6InstallVclFmx.groupproj // VCL and FMX components for Windows

Delphi XE7       :  DXe7InstallVcl.groupproj    // VCL components for Windows
Delphi XE7       :  DXe7InstallFmx.groupproj    // FMX components for Windows
Delphi XE7       :  DXe7InstallVclFmx.groupproj // VCL and FMX components for Windows

Delphi XE8       :  DXe8InstallVcl.groupproj    // VCL components for Windows
Delphi XE8       :  DXe8InstallFmx.groupproj    // FMX components for Windows
Delphi XE8       :  DXe8InstallVclFmx.groupproj // VCL and FMX components for Windows

Delphi 10        : D10SInstallVcl.groupproj    // VCL components for Windows
Delphi 10        : D10SInstallFmx.groupproj    // FMX components for Windows
Delphi 10        : D10SInstallVclFmx.groupproj // VCL and FMX components for Windows

Delphi 10.1      : D101InstallVcl.groupproj    // VCL components for Windows
Delphi 10.1      : D101InstallFmx.groupproj    // FMX components for Windows
Delphi 10.1      : D101InstallVclFmx.groupproj // VCL and FMX components for Windows

Delphi 10.2      : D102InstallVcl.groupproj    // VCL components for Windows
Delphi 10.2      : D102InstallFmx.groupproj    // FMX components for Windows
Delphi 10.2      : D102InstallVclFmx.groupproj // VCL and FMX components for Windows

Delphi 10.3      : D103InstallVcl.groupproj    // VCL components for Windows
Delphi 10.3      : D103InstallFmx.groupproj    // FMX components for Windows
Delphi 10.3      : D103InstallVclFmx.groupproj // VCL and FMX components for Windows

Sorry, no C++ Builder install groups are available with V9.1 and later for older
compIlers than 10.4, you will need to use ICS V9.0 instead or create them yourself.

1 - Do a File/Open Project, navigate to the Install directory, select the correct
file and open it. The project manager view should now display various package
projects, some run-time and some design-time package. The run-time package name
contains the "Run" suffix. The design-time package name contains the "Design"
suffix.

2 - Select and Build each package in turn, for each platform and build
configuration required, usually four times.  Note the Common package must always
be built and installed, in addition to VCL or FMX, or both.

3 - Or click the 'Show Build Groups Pane' button, select an Active Group and click
'Build the current project group' which builds all packages for all platforms and
configurations together.

4 - Select and Install the design-time packages one at a time.

After a few seconds, you should have a dialog box telling you the package has
been installed with a bunch of new components registered in the Tool Palette
under "Overbyte ICS" and "Overbyte ICS SSL". Then do a "Save All" and a "Close All".

5 - Two or three packages should now appear n the Delphi Packages list, called
'Overbyte ICS Common/FMX/VCL Design-Time Package for Delphi'.

6 - Note the packages all have 'Post-build events' to copy install files around,
and sometimes these may give errors during building, if these files can not be
found or certain directories are missing.  This does not means the packages are
not built correctly, just that some things may not work as expected.  Specifically,
the Common packages copies all files from .\ICS-OpenSSL to c:\ProgramData\ICS-OpenSSL\
and may fail if running applications are using OpenSSL files in that directory.  The
VCL and FMX packages copy two .DFM files from the .\Source directory to .\Lib so
they can be found later.

7 - Add the ICS library path so applications can find the DCU files, and perhaps
source files.

8 - For Delphi XE and earlier, use Tools / Options / Delphi Options / Library - Win32 /
Library Path, and add the .\Lib subdirectory according to version, ie
.\Lib\Debug\Win32\D2007 for Delphi 2007, replacing the first . with the install
directory, if not done previously.  If you modify the ICS source files, also add
.\Source.

9 - For Delphi XE2 and later, separate Win32 and Win64 paths need specifying, use
Tools / Options / Language / Delphi/ Library / select platform as Windows 32-bit or
64-bit, or MacOs 64-bit, and add the .\Lib subdirectory according to version,  ie
.\Lib\Debug\Win64\D103 for Delphi 10.3 64-bit, or .\Lib\Debug\Win32\23.0 for Delphi
12 32-bit.  Note Delphi 10.4 and later use release studio version, so 21.0 for
10.4, 22.0 for Delphi 11, 11.1 and 11.2, and 23.0 for Delphi 12, 12.1, etc.


ALTERNATE INSTALLATION USING THE PACKAGE PROJECT FILES:

For each Delphi and C++ Builder version several package project files exist in the
.\Packages directory, run-time and design-time package project files.  These files
are all referenced in the groupproj files in the .\Install directory, but may be
built and installed separately if so desired.

PACKAGE PROJECT FILE NAMES - Common, then FMX and/or VCL:

Delphi 10.4/11/12 FMX/VCL      : IcsCommonNewRun.dproj, IcsCommonNewDesign.dproj
Delphi 10.4/11/12 VCL          : IcsVclNewRun.dproj, IcsVclNewDesign.dproj
Delphi 10.4/11/12 FMX          : IcsFmxNewRun.dproj, IcsFmxNewDesign.dproj

C++ Builder 10.4/11/12 FMX/VCL : IcsCommonCBNewRun.cbproj, IcsCommonCBNewDesign.cbproj
C++ Builder 10.4/11/12 FMX     : IcsFmxCBNewRun.cbproj, IcsFmxCBNewDesign.cbproj
C++ Builder 10.4/11/12 VCL     : IcsVclCBNewRun.cbproj, IcsVclCBNewDesign.cbproj


PACKAGE PROJECT FILE NAMES - VCL:

Delphi 7         :  OverbyteIcsD7Run.dpk, OverbyteIcsD7Design.dpk
Delphi 2006      :  OverbyteIcsD2006Run.bdsproj, OverbyteIcsD2006Design.bdsproj
Delphi 2007      :  OverbyteIcsD2007Run.dproj, OverbyteIcsD2007Design.dproj
Delphi 2009      :  OverbyteIcsD2009Run.dproj, OverbyteIcsD2009Design.dproj
Delphi 2010      :  OverbyteIcsD2010Run.dproj, OverbyteIcsD2010Design.dproj
Delphi XE        :  OverbyteIcsDXeRun.dproj, OverbyteIcsDXeDesign.dproj

PACKAGE PROJECT FILE NAMES - Common, then FMX and/or VCL:

Delphi XE2 FMX/VCL      :  IcsCommonDXe2Run.dproj, IcsCommonDXe2Design.dproj
Delphi XE2 VCL          :  IcsVclDXe2Run.dproj, IcsVclDXe2Design.dproj
Delphi XE2 FMX          :  IcsFmxDXe2Run.dproj, IcsFmxDXe2Design.dproj

Delphi XE3 FMX/VCL      :  IcsCommonDXe3Run.dproj, IcsCommonDXe3Design.dproj
Delphi XE3 VCL          :  IcsVclDXe3Run.dproj, IcsVclDXe3Design.dproj
Delphi XE3 FMX          :  IcsFmxDXe3Run.dproj, IcsFmxDXe3Design.dproj

Delphi XE4 FMX/VCL      :  IcsCommonDXe4Run.dproj, IcsCommonDXe4Design.dproj
Delphi XE4 VCL          :  IcsVclDXe4Run.dproj, IcsVclDXe4Design.dproj
Delphi XE4 FMX          :  IcsFmxDXe4Run.dproj, IcsFmxDXe4Design.dproj

Delphi XE5 FMX/VCL      :  IcsCommonDXe5Run.dproj, IcsCommonDXe5Design.dproj
Delphi XE5 VCL          :  IcsVclDXe5Run.dproj, IcsVclDXe5Design.dproj
Delphi XE5 FMX          :  IcsFmxDXe5Run.dproj, IcsFmxDXe5Design.dproj

Delphi XE6 FMX/VCL      :  IcsCommonDXe6Run.dproj, IcsCommonDXe6Design.dproj
Delphi XE6 VCL          :  IcsVclDXe6Run.dproj, IcsVclDXe6Design.dproj
Delphi XE6 FMX          :  IcsFmxDXe6Run.dproj, IcsFmxDXe6Design.dproj

Delphi XE7 FMX/VCL      :  IcsCommonDXe7Run.dproj, IcsCommonDXe7Design.dproj
Delphi XE7 VCL          :  IcsVclDXe7Run.dproj, IcsVclDXe7Design.dproj
Delphi XE7 FMX          :  IcsFmxDXe7Run.dproj, IcsFmxDXe7Design.dproj

Delphi XE8 FMX/VCL      :  IcsCommonDXe8Run.dproj, IcsCommonDXe8Design.dproj
Delphi XE8 VCL          :  IcsVclDXe8Run.dproj, IcsVclDXe8Design.dproj
Delphi XE8 FMX          :  IcsFmxDXe8Run.dproj, IcsFmxDXe8Design.dproj

Delphi 10 Seattle FMX/VCL: IcsCommonD10SRun.dproj, IcsCommonD10SDesign.dproj
Delphi 10 Seattle VCL   :  IcsVclD10SRun.dproj, IcsVclD10SDesign.dproj
Delphi 10 Seattle FMX   :  IcsFmxD10SRun.dproj, IcsFmxD10SDesign.dproj

Delphi 10.1 Berlin FMX/VCL: IcsCommonD101Run.dproj, IcsCommonD101Design.dproj
Delphi 10.1 Berlin VCL  :  IcsVclD101Run.dproj, IcsVclD101Design.dproj
Delphi 10.1 Berlin FMX  :  IcsFmxD101Run.dproj, IcsFmxD101Design.dproj

Delphi 10.2 Tokyo FMX/VCL: IcsCommonD102Run.dproj, IcsCommonD102Design.dproj
Delphi 10.2 Tokyo VCL   :  IcsVclD102Run.dproj, IcsVclD102Design.dproj
Delphi 10.2 Tokyo FMX   :  IcsFmxD102Run.dproj, IcsFmxD102Design.dproj

Delphi 10.3 Rio FMX/VCL :  IcsCommonD103Run.dproj, IcsCommonD103Design.dproj
Delphi 10.3 Rio VCL     :  IcsVclD103Run.dproj, IcsVclD103Design.dproj
Delphi 10.3 Rio FMX     :  IcsFmxD103Run.dproj, IcsFmxD103Design.dproj



SAMPLE DELPHI PROJECTS INSTALLATION

Once the package is installed, you may open the sample projects. There are about 95
samples are split into several directories, with a project group.

.\demos-delphi-vcl                    VCL samples for Windows
.\demos-delphi-extra                  VCL samples that need third party components to build
.\demos-delphi-fmx                    FMX samples for Windows, not yet tested on MacOS
.\demos-delphi-mobile                 Empty, for the future
.\demos-cpp-vcl                       Old C++ samples that have not been tested for 10 years
.\demos-data                          Data files for samples, such as web pages

Full details of the individual sample projects are shown later in this document.

Each directory has a group project file with the same name as the directory, that
includes all the projects in that directory, so demos-delphi-vcl.groupproj for the
main samples directory.  To compile all samples in the group at once, do  Project /
Build all projects. This may take a few minutes.  For legacy compilers, open the
demos-delphi-vcl-legacy.bpg project file instead, or demos-delphi-vcl-legacy.groupproj.

Beware the sample project files (.dproj) supplied are built with modern compilers,
and can not be opened by legacy compilers due to new platforms and features.  So
for Delphi XE and earlier (and maybe some other XE versions), before opening a
group or application project, you MUST delete all .dproj sample files.  When you
open the project, the .dproj file will be automatically recreated from the .dpr
project file by Delphi.  If you attempt to open a new .dproj file with a legacy
Delphi compiler, it will simply give an XML error and not attempt to rebuild the
project file.

Please be aware the C++ Builder packages and samples have not been updated for some
years and are untested with V9.1 and later.  They are included here for existing users.


SSL/TLS Optional DEFINES
------------------------

Actual use of SSL in your applications also requires OpenSSL files:

libcrypto-3.dll or libcrypto-3-x64.dll
libssl-3.dll or libssl-3-x64.dll
legacy.dll or legacy-x64.dll

The legacy DLLs are only needed if support for old algorithms is needed, but this
includes most password protected PFX/PCS12 certificates.

Distribution of the ICS OpenSSL files changed with V9.1 and later.  Earlier ICS
versions required the OpenSSL DLLs to be distributed with applications, and a
root CA bundle file to verify SSL/TLS connections, and these needed to be loaded
using code.  There was little standardisation over where the OpenSSL DLLs were
located, applications tended to keep their own copies alongside other executables,
leading to multiple DLL copies and needing the public variable GSSL_DLL_DIR set to
a specific directory before OpenSSL was loaded.  Likewise, root CA bundle
directories had to be distributed with applications and loaded with code.

ICS V9.1 and later allows five different ways of loading OpenSSL:

1 - DLLs linked into application as resource files
2 - DLLs loaded from common directory C:\ProgramData\ICS-OpenSSL\
3 - OpenSSL DCU linked into application using commercial YuOpenSSL
4 - DLLs loaded from location specified in public variable GSSL_DLL_DIR
5 - DLLs loaded according to path, may be found anywhere on PC

Which method ICS uses to load OpenSSL depends upon several defines in the
.\Source\Include\OverbyteIcsDefs.inc file:

{$DEFINE USE_SSL} - default enabled, link OpenSSL into all components, but does
not require SSL to be used.

{$DEFINE OpenSSL_Resource_Files} - default enabled, link OpenSSL DLLs as resource file
into applications, and extract them to shell path CSIDL_COMMON_APPDATA and sub-directory
"ICS-OpenSSL" with a version subdirectory, ie C:\ProgramData\ICS-OpenSSL\3012\ . This
happens only once if the files have not already been extracted.  Ignored for YuOpenSSL.

{$DEFINE OpenSSL_34} - if OpenSSL_Resource_Files is enabled, determines which major and
minor version of OpenSSL is linked into the application, 3016 is 3.0.16, or 32, 34 or 34.
ICS is currently distributed with OpenSSL 3.0, 3.2, 3.3 and 3.4, the latest patch of each
version so 3.4.1, 3.3.3, 3.2.4 and 3.0.16, the resources files are in .\Source\,
LibV34OpenSSL32.RES for 3.4 Win32, total eight resource files, ICS automatically links
Win32 or Win64 RES files. Note the resource files only have the major OpenSSL version,
3. not the minor version, to avoid changing the file names for minor releases.

Note OpenSSL Versions: Although ICS supports (currently) four different OpenSSL versions,
ICS does not yet use any new features added to OpenSSL 3.1 or later, which are mostly
related to FIPS and QUIC for HTTP/3, so using OpenSSL 3.0 is perfectly fine, but it is
essential to always uses the latest sub-version with security fixes.

{$DEFINE OpenSSL_ProgramData} - default enabled, but ignored if OpenSSL_Resource_Files or
YuOpenSSL enabled.  Causes ICS to load OpenSSL DLLs from C:\ProgramData\ICS-OpenSSL\,
an alias for C:\Users\All Users\ICS-OpenSSL. ICS is distributed with Win32 and Win64 DLLs
for 3.4.1 in .\ICS-OpenSSL which are copied there when building the IcsCommonXXRun
package. Note there is no version sub-directory so no version choice.  If enabled,
overrides the public variable GSSL_DLL_DIR which some applications set to load OpenSSL
from a known directory.

{$DEFINE YuOpenSSL} - default disabled.  If enabled, compiles the OpenSSL code as a DCU
directly into binaries so the OpenSSL are not needed, YuOpenSSL is a commercial product
from https://www.yunqa.de/.  OpenSSL 3.0 is available for YuOpenSSL, not currently any
later versions.

NOTE: if defines OpenSSL_Resource_Files, OpenSSL_ProgramData and YuOpenSSL are all
disabled or missing, ICS loads OpenSSL from the directory specified in the public
variable GSSL_DLL_DIR, which is typically set to the application directory.  If blank,
Windows will search the path for any OpenSSL 3 DLLs, anywhere.

{$DEFINE OpenSSL_CA_Bundle_Small} - default enabled, links a root certificate authority
bundle as a resource file into applications, other options are Medium and Large. CA
bundles are needed to verify that SSL/TLS certificates are issued by trusted authorities,
the resources files are in .\Source\, sslRootCACertsBundle.RES (OpenSSL_CA_Bundle_Small),
TrustedCaBundle.RES (OpenSSL_CA_Bundle_Medium) and RootCaCertsBundle.RES
(OpenSSL_CA_Bundle_Large).

{$DEFINE OpenSSL_AutoLoad_CA_Bundle} - default enabled. With ICS V9.1 and later, a
common IcsSslRootCAStore component is created at application start-up, if this define is
enabled OpenSSL will be loaded followed by the root CA bundle RES file according to
define OpenSSL_CA_Bundle_Small/Medium/Large.  This means OpenSSL is available for all
components, without it needing to be loaded again, perhaps repeatedly, and multiple
components can share the IcsSslRootCAStore component without needing to load their
own CA bundles.  If this defined is not enabled, SslRootCAStore.Initialise may be called
by the application to load OpenSSL and the CA bundle, which is done automatically by
SslContext.InitContext if not done previously.

{$DEFINE AUTO_X509_CERTS} - default enabled.  This define enables automatic SSL/TLS
ordering from Let's Encrypt in SocketServer and other servers.  Unfortunately this adds
a lot of other units, HTTPS REST, Json, OAuth2, etc, increasing the size of server
applications, so it may be disabled to make server EXE files smaller if certificates
are obtained and installed manually.

{$DEFINE OpenSSL_Check_Signed} and {$DEFINE OpenSSL_Check_SignCert} - default enabled.
These defines check the OpenSSL DLLs are digitally signed during loading and stop if
corrupted or unsigned, optionally the signing certificate can be verified as well but
this requires COM to be started which may be slow.  Added with V9.3.

{$DEFINE MSCRYPT_Clients}, {$DEFINE MSCRYPT_Servers} and {$DEFINE MSCRYPT_Tools) -
default enabled.  Allow greater control over whether the Windows Certificate Store is
is available in diferent types of components. Undefining MSCRYPT_Clients stops client
SSL handshake verification using the Windows Store (CertVerMethod = CertVerWinStore)
which is not necessary since ICS now includes it's own Root CA Bundle (see above).
Undefining MSCRYPT_Servers stops IcsHosts in servers accessing server certificates
in the Windows Store. Undefining MSCRYPT_Tools stops TSslX509Certs saving certificates
to the Windows Store, and many PemTools functions.   These defines are disabled for
non-Windows platforms and for C++ Builder which has bugs. Added with V9.3.

Except when using the OpenSSL_AutoLoad_CA_Bundle define, OpenSSL still needs to be loaded
before any SSL/TLS functionality can be used. This is done automatically by TSslContext
and some other components that use OpenSSL, but this means SSL errors like the DLLs not
being found may not be raised until a web page is accessed, etc.  So it is generally
better to load OpenSSL early on in your application, when errors are easier to handle
There is a function IcsReportOpenSSLVer that returns the OpenSSL version loaded and where
it was loaded from, to help debug loading problems.

When using the OpenSSL_AutoLoad_CA_Bundle define, if the OpenSSL legacy.dll is needed to
support old algorithms, which includes most password protected PFX/PCS12 certificates,
it must be loaded using IcsSslLoadProviders(True, False); since it is not possible to set
the GSSLEAY_LOAD_LEGACY global variable early enough.

By default the SSL code is compiled into the run-time package and additional SSL-
enabled components are installed. In order to not compile the SSL code into the
run-time package and to not install the SSL-Enabled components you need to remove
the conditional define USE_SSL from both the run-time and design-time package and
from the \OverbyteIcsDefs.inc file. Beware you won't be able to open the
demos-delphi-vcl samples group because most them require SSL components.

However if you do not build your applications with run-time packages it is
recommended to build the packages with default settings. The SSL code will the
be compiled into your applications depending on whether the conditional define
USE_SSL is set in the project options or not (this requires having the .\Source
directory in either in the library path or in projects Search path).

Note the use of USE_SSL is historical from when SSL was a chargeable extra and
future versions of ICS may always build with SSL to reduce the heavy support
burden of building with and without SSL.  Most modern applications need SSL.
All new ICS components in the last few years have been SSL only.


'ICS Root CA' and Extra Certificates
------------------------------------

All SSL/TLS servers need a certificate and private key to start, even when testing.
Previously ICS supplied some self signed certificates for testing, and also created
such certificates automatically if they were missing or if the server was about to
order a Let's Encrypt certificate.  Accessing such servers for testing using browsers
raised various warnings.

ICS now has it's own SSL root certificate 'ICS Root CA' and two intermediates, 'ICS
Intermediate' and 'ICS Intermediate Short', the last of which includes a private key
so can be used to automatically sign new certificates by ICS server applications,
rather than just self signed certificates as before. If the 'ICS Root CA' certificate
is installed in the Window Store and browser stores, it should stop certificate
warnings appearing. ICS applications automatically trust the ICS root, so will give
no warnings. The short intermediate has a maximum 100 day expiry, so new versions will
be issued regularly. There is a single function CreateSelfSignCertEx that created
signed certificates, and another IcsInstallIcsRoot that installs the ICS root into
the Windows Store, so easy to use. It is possible to replace the ICS root with your
own private root certificate and have servers create their own certificates against
that root, for internal networks.

The ICS SSL root certificate is loaded automatically with the SslRootCAStore.Initialise
function that loads the CA Root Store, and with the define OpenSSL_AutoLoad_CA_Bundle,
to verify any ICS issued certificates.  The SslRootCAStore.Initialise functions also
tries to load the file C:\ProgramData\ICS-OpenSSL\ExtraRootCABundle.pem which is an
optional private root bundle that can be used for private customer or developer root
CA certificates, in PEM format.


SSL/TLS Updating to V9.1 and later
----------------------------------

There are several new units with V9.1.

Opening project forms with the IDE should automatically add any new units required,
but OverbyteIcsTypes and OverbyteIcsSslBase may need adding manually if ICS components
are created in code. Applications using TSslContext now need OverbyteIcsSslBase, units
that reference TX509Base or TX509List mostly for the OnSslHandshakeDone event, may
also need OverbyteIcsSslBase adding.

The other new units are OverbyteIcsHtmlUtils (for TextToHtmlText, IcsHtmlValuesToUnicode,
IcsFindHtmlCharset, IcsFindHtmlCodepage, IcsContentCodepage and IcsHtmlToStr), OverbyteIcsDnsHttps
(for TDnsQueryHttp and IcsDomNameCacheHttps) and OverbyteIcsSslUtils (for TOcspHttp).
Applications that use IcsExtractURLEncodedValue, ExtractURLEncodedParamList or GetCookieValue
may need OverbyteIcsUrl adding to projects.

When updating projects using a TSslContext component, setting the new property UseSharedCAStore
to True causes the properties CAFile, CALines and CAPath to be ignored, and the new
IcsSslRootCAStore component will be used instead, being automatically initialised if not done at
program start-up.  Don't use UseSharedCAStore for server components.

High level ICS components such as TSslHttpRest that have an internal TSslContext component all
set UseSharedCAStore and ignore properties like SslRootFile to load a root CA bundle.  If a
specific bundle is required it may be loaded to IcsSslRootCAStore.

With V9,1, the global variables GSSLEAY_DLL_IgnoreNew and GSSLEAY_DLL_IgnoreOld
are ignored since only different minor versions of OpenSSL 3 are supported.

Prior to V9.3, when creating new ICS applications by dropping components on forms and creating
events, the compiler often returns errors about missing types requiring location and adding of
those missing units.  With V9.3, adding OverbyteIcsTypes will resolve most missing type and
constant errors, while adding OverbyteIcsSslBase should resolve SSL errors.

While this change will benefit new projects, OverbyteIcsTypes will be needed by most existing
projects but should be added automatically by opening forms in the IDE.  OverbyteIcsDefs.inc
will need to be updated to the latest version for full backward compatibility.  Note there are
no new units with these changes, OverbyteIcsTypes has always existed primarily with newer types
for older compilers.

One benefit of this change is removing dependence on several units for many components and
applications, it may be possible to remove OverbyteIcsWinsock, OverbyteIcsLIBEAY,
OverbyteIcsSSLEAY and OverbyteIcsLogger from most applications, and OverbyteIcsWinCrypt,
OverbyteIcsSslX509Utils, OverbyteIcsHttpProt, OverbyteIcsSmtpProt and OverbyteIcsFtpCli from
applications using newer high level components like OverbyteIcsSslHttpRest.


SSL/TLS Downloads
-----------------

Note OpenSSL 3.0 and later only support Windows Vista and later, and Windows Server
2008 and later, not Windows XP.

Other OpenSSL files may be downloaded from:

https://wiki.overbyte.eu/wiki/index.php/ICS_Download

The newer zip files contain both DLLs and RES files, so may be used to manually
update ICS for newer OpenSSL versions without installing a new ICS distribution,

Note that OpenSSL support for 1.0.2 and 1.1.0 ceased in 2019 with no more security
fixes, and ICS V8.66 and later no longer support them. Support for 1.1.1 ceased in
2023 and V9.1 no longer supports it.  OpenSSL3.1.x ceases support in less than a year
and has two newer releases, so we've stopped building binaries for it.

ICS also supports YuOpenSSL which provides OpenSSL in a pre-built DCU statically
linked into applications, rather than using external OpenSSL DLLs. This make
application distribution more reliable since it can no fail by users deleting the
DLLs or copying incompatible versions into the directory.  YuOpenSSL is a commercial
product from https://www.yunqa.de/ and is supplied as separate compiled DCUs for
Delphi 5 to 12. DEFINE YuOpenSSL in Include\OverbyteIcsDefs.inc determines whether
the DCU is linked or the external DLLs.  Note only one version of OpenSSL can be
linked with YuOpenSSL, whereas different DLLs can be supported.  Apart from setting
the define and adding a path to YuOpenSSL.dcu, no other application code changes
are needed unless you check or report the DLL version directly.



Available VCL Components
------------------------

The following is a list of the files that should be installed in order to
properly add all of the available components in this collection:

> OverbyteIcsAppMonCli.pas     ICS Application Monitor client component
> OverbyteIcsAppMonSrv.pas     ICS Application Monitor server component
> OverbyteIcsCharsetComboBox.pas Provides easy MIME charset selection
> OverbyteIcsDnsQuery          DNS lookup component, TIcsDomNameCache - useful for getting MX records
> OverbyteIcsDnsQueryHttps     DNS lookup component with HTTPS protocol
> OverbyteIcsDprUpdFix.pas     IDE plugin for Delphi 2009 and 2010 to update old projects
> OverbyteIcsEmulVT.pas        ANSI terminal emulation in a control
> OverbyteIcsFileCopy.pas      Indexing, copying and deleting of multiple file directories
> OverbyteIcsFileCopyW.pas     Same as OverbyteIcsFileCopy but Unicode for Delphi 2007.
> OverbyteIcsFingCli.pas       FINGER client protocol - Find information about user
> OverbyteIcsFtpCli.pas        FTP client protocol - file transfer
> OverbyteIcsFtpCliW.pas       Same as OverbyteIcsFtpCli but Unicode for Delphi 2007.
> OverbyteIcsFtpMulti.pas      FTP client that indexes, uploads or downloads multiple files
> OverbyteIcsFtpMultiW.pas     Same as OverbyteIcsFtpMulti but Unicode for Delphi 2007.
> OverbyteIcsFtpSrv.pas        FTP server protocol - file transfer
> OverbyteIcsFtpSrvT.pas       FTP server protocol - helpers
> OverbyteIcsFtpSrvW.pas       Same as OverbyteIcsFtpSrvW but Unicode for Delphi 2007.
> OverbyteIcsHttpAppServer.pas HTTP server protocol - used to build advanced web servers
> OverbyteIcsHttpMulti.pas     HTTP client that downloads multiple files from a list or by parsing web links
> OverbyteIcsHttpMultiW.pas    Same as OverbyteIcsHttpMulti but Unicode for Delphi 2007.
> OverbyteIcsHttpProt.pas      HTTP client protocol - used by the web
> OverbyteIcsHttpSrv.pas       HTTP server protocol - used to build web servers
> OverbyteIcsIpStreamLog.pas   IP stream logging, using TCP Client or Server, UDP Client or Server, sends simple text
> OverbyteIcsLogger.pas        A component to log information
> OverbyteIcsMailQueue.pas     SMTP Mail Queue with extended retries, multiple SMTP servers or MX look up
> OverbyteIcsMimeDec.pas       MIME component - decode file attach, use with POP3
> OverbyteIcsMQTT.pas          MQ Telemetry Transport message queuing service
> OverbyteIcsMultiProgressBar.pas A segmented progress bar
> OverbyteIcsMultipartFtpDownloader.pas   FTP client protocol - download one file using simultaneous connections
> OverbyteIcsMultipartHttpDownloader.pas  HTTP client protocol - download one file using simultaneous connections
> OverbyteIcsNntpCli.pas       NNTP client protocol - send and receive newsgroups messages
> OverbyteIcsOAuthFormFmx.pas  OAuth2 Embedded Browser for FMX
> OverbyteIcsOAuthFormVcl.pas  OAuth2 Embedded Browser for VCL
> OverbyteIcsPing.pas          ICMP echo protocol - ping a host
> OverbyteIcsPop3Prot.pas      POP3 client protocol - get mail from mail server
> OverbyteIcsProxy.pas         Proxy server protocol - HTTP forward and reverse proxy, and others
> OverbyteIcsReg.pas           Register design components
> OverbyteIcsSmtpProt.pas      SMTP client protocol - send mail to server
> OverbyteIcsSmtpSrv.pas       SMTP server protocol - receive mail from client
> OverbyteIcsSnmpCli.pas       SNMP client protocol - network management
> OverbyteIcsSnmpMsgs.pas      SNMP client protocol - message helper
> OverbyteIcsSntp.pas          Time server and client supporting SNTP time protocol
> OverbyteIcsSslHttpOAuth.pas  HTTPS OAuth2, and cloud components for email and Twitter
> OverbyteIcsSslHttpRest.pas   HTTPS REST functions, descends from THttpCli, Send SMS and DNS over HTTPS
> OverbyteIcsSysLogClient.pas  Syslog Client Protocol - receive syslog messages
> OverbyteIcsSysLogDefs.pas    Syslog Protocol - helpers
> OverbyteIcsSysLogServer.pas  Syslog Server Protocol - send syslog messages
> OverbyteIcsTnCnx.pas         TELNET client protocol - terminal emulation protocol
> OverbyteIcsTnEmulVT.pas      TELNET and ANSI terminal emulation combined
> OverbyteIcsTnOptFrm.pas      TELNET Client configuration form
> OverbyteIcsTnScript.pas      TELNET client protocol - with automation
> OverbyteIcsWebSockets.pas    WebSocket Server protocol - old version
> OverbyteIcsWebSocketCli.pas  WebSocket Client protocol
> OverbyteIcsWebSocketsrv.pas  WebSocket Server protocol - new version
> OverbyteIcsWSocket.pas       Winsock component - TCP, UDP, DNS
> OverbyteIcsWSocketE.pas      Register procedure and property editor for TWSocket
> OverbyteIcsWSocketS.pas      Winsock component for building servers
> OverbyteIcsWSocketTS.pas     Winsock component for building multithreaded servers
> OverbyteIcsWhoisCli.pas      Whois protocol client

- The following list support and utilities units:
> OverbyteIcsAsn1Utils.pas     ASN1 utilities (for TSnmpClient component)
> OverbyteIcsAvlTrees.pas      Implements a fast cache-like data storage
> OverbyteIcsBlacklist.pas     Blacklisting of malicious IP addresses, logging functions
> OverbyteIcsCRC.pas           32 bit CRC computation
> OverbyteIcsCharsetUtils.pas  MIME-charset functions
> OverbyteIcsCookies.pas       Client Cookie Handling
> OverbyteIcsCsc.pas           character set routines
> OverbyteIcsDES.pas           Implementation of the Data Encryption Standard (DES)
> OverbyteIcsDigestAuth.pas    HTTP Digest Access Authentication
> OverbyteIcsFormDataDecoder.pas Decode a MIME data block as generated by a HTML form
> OverbyteIcsFtpSrvWT.pas      Same as OverbyteIcsFtpSrvWT but Unicode for Delphi 2007.
> OverbyteIcsHtmlPars.pas      HTML web page parser
> OverbyteIcsHtmlUtils.pas     HTML to text, and vice versa, conversions.
> OverbyteIcsHttpCCodZLib.pas  Supports GZIP coding for HttpContCod
> OverbyteIcsHttpContCod.pas   HTTP Content Coding support, uses extra units
> OverbyteIcsIcmp.pas          ICMP protocol support, used by the PING component
> OverbyteIcsIconv.pas         Headers for iconv library (LGPL)
> OverbyteIcsLIBEAY.pas        Delphi encapsulation for libeay32.dll and libcrypto-1_1.dll (OpenSSL)
> OverbyteIcsMD4.pas           Implementation of the MD4 Message-Digest Algorithm
> OverbyteIcsMD5.pas           Implementation of the MD5 Message-Digest Algorithm
> OverbyteIcsMLang.pas         A few header translations from MS mlang.h
> OverbyteIcsMonCommon.pas     Internet monitoring common headers and structures
> OverbyteIcsMonNdis.pas       Npcap NDIS Packet Capture Driver headers.
> OverbyteIcsMonPcap.pas       Internet monitoring using Npcap NDIS driver
> OverbyteIcsMonSocket.pas     Internet monitoring using raw sockets
> OverbyteIcsMimeUtil.pas      Support routines for MIME standard
> OverbyteIcsNtlmMsgs.pas      Client NTLM authentication messages used within HTTP protocol
> OverbyteIcsNtlmSsp.pas       Server NTLM authentication of user credentials using Windows SSPI
> OverbyteIcsOneTimePw.pas     One Time Password support functions, used by FTP
> OverbyteIcsSHA1.pas          Implementation of US Secure Hash Algorithm 1 (SHA1)
> OverbyteIcsSSLEAY.pas        Delphi encapsulation for ssleay32.dll and libssl-1_1.dll (OpenSSL)
> OverbyteIcsSocketUtils.pas   Cross platform socket utilities for ICS
> OverbyteIcsSslBase.pas       TSslContext, TX509Base and TX509List for SSL/TLS X509 certificates.
> OverbyteIcsSslJose.pas       JOSE - Json Object Signing and Encryption
> OverbyteIcsSslSessionCache.pas  A very fast external SSL-session-cache component
> OverbyteIcsSslUtils.pas      TOcspHttp and SSL other utility functions.
> OverbyteIcsSslX509Certs.pas  Automatically download SSL X509 certificates from Let's Encrypt and CertCentre AG
> OverbyteIcsSslX509Utils.pas  SSL key and X509 certification creation
> OverbyteIcsSspi.pas          A few header translations from MS sspi.h and security.h
> OverbyteIcsStreams.pas       Fast streams for ICS
> OverbyteIcsThreadTimer.pas   A custom timer class using custom timer messages from one or more threads
> OverbyteIcsTicks64.pas       GetTickCount64 support for all versions of Windows
> OverbyteIcsTimeList.pas      List of items with expiry times, used for WebSessions
> OverbyteIcsTypes.pas         Common types, mainly for backward compiler compatibility
> OverbyteIcsURL.pas           Support routines for URL handling
> OverbyteIcsUtils.pas         Vast number of common utilities, many supporting Unicode for D7/2007
> OverbyteIcsWSockBuf.pas      FIFO buffers for TWSocket
> OverbyteIcsWebSession.pas    Web session support for THttpAppSrv and MidWare
> OverbyteIcsWinnls.pas        A few header translations for Unicode Normalization in winnls.h
> OverbyteIcsWinsock.pas       Some Winsock initialisations
> OverbyteIcsWMI.pas           WMI support functions, setting IP addresses, controlling DNS server.
> OverbyteIcsWndControl.pas    A class that encapsulates a windows message queue and a message map
> OverbyteIcsZLibDll.pas       Zlib support, interface to external zlib.dll functions
> OverbyteIcsZLibObj.pas       Zlib support, interface to zlib linked C OBJ functions
> OverbyteIcsZlibHigh.pas      Zlib support, high level interface for compression and decompression
> WbemScripting_TLB.pas        WMI API headers.


FireMonkey FMX Cross Platform Support:
-------------------------------------

For Delphi and C++ Builder XE2 and later, FireMonkey or FMX Desktop applications are an alternate
to VCL Forms applications, supporting cross platforms of Windows 32-bit and 64-bit and Mac
OS X (and perhaps other platforms in future).  FireMonkey uses different visual components
to VCL, while some non-visual components can be used for both VCL and FMX projects, while
other components need special versions, such as ICS.

OverbyteIcsBlacklist               -> Ics.Fmx.OverbyteIcsBlacklist.pas
OverbyteIcsCharsetComboBox         -> Ics.Fmx.OverbyteIcsCharsetComboBox.pas
OverbyteIcsDnsQuery                -> Ics.Fmx.OverbyteIcsDnsQuery.pas
OverbyteIcsFileCopy                -> Ics.Fmx.OverbyteIcsFileCopy.pas
OverbyteIcsFingCli                 -> Ics.Fmx.OverbyteIcsFingCli.pas
OverbyteIcsFtpCli                  -> Ics.Fmx.OverbyteIcsFtpCli
OverbyteIcsFtpMulti                -> Ics.Fmx.OverbyteIcsFtpMulti.pas
OverbyteIcsFtpSrv                  -> Ics.Fmx.OverbyteIcsFtpSrv
OverbyteIcsHttpAppServer           -> Ics.Fmx.OverbyteIcsHttpAppServer.pas
OverbyteIcsHttpMulti               -> Ics.Fmx.OverbyteIcsHttpMulti.pas
OverbyteIcsHttpProt                -> Ics.Fmx.OverbyteIcsHttpProt
OverbyteIcsHttpSrv                 -> Ics.Fmx.OverbyteIcsHttpSrv.pas
OverbyteIcsIcmp                    -> Ics.Fmx.OverbyteIcsIcmp.pas
OverbyteIcsIpStreamLog             -> Ics.Fmx.OverbyteIcsIpStreamLog.pas
OverbyteIcsMailQueue               -> Ics.Fmx.OverbyteIcsMailQueue.pas
OverbyteIcsMsSslUtils              -> Ics.Fmx.OverbyteIcsMsSslUtils.pas
OverbyteIcsMultipartFtpDownloader  -> Ics.Fmx.OverbyteIcsMultipartFtpDownloader.pas
OverbyteIcsMultipartHttpDownloader -> Ics.Fmx.OverbyteIcsMultipartHttpDownloader.pas
OverbyteIcsNntpCli                 -> Ics.Fmx.OverbyteIcsNntpCli.pas
OverbyteIcsPing                    -> Ics.Fmx.OverbyteIcsPing.pas
OverbyteIcsPop3Prot                -> Ics.Fmx.OverbyteIcsPop3Prot.pas
OverbyteIcsProxy                   -> Ics.Fmx.OverbyteIcsProxy.pas
OverbyteIcsSmtpProt                -> Ics.Fmx.OverbyteIcsSmtpProt.pas
OverbyteIcsSntp                    -> Ics.Fmx.OverbyteIcsSntp.pas
OverbyteIcsSocketUtils             -> Ics.Fmx.OverbyteIcsSocketUtils.pas
OverbyteIcsSslHttpRest             -> Ics.Fmx.OverbyteIcsSslHttpRest.pas
OverbyteIcsSslJose                 -> Ics.Fmx.OverbyteIcsSslJose.pas
OverbyteIcsSslSessionCache         -> Ics.Fmx.OverbyteIcsSslSessionCache.pas
OverbyteIcsSslX509Certs            -> Ics.Fmx.OverbyteIcsSslX509Certs.pas
OverbyteIcsSslX509Utils            -> Ics.Fmx.OverbyteIcsSslX509Utils.pas
OverbyteIcsThreadTimer             -> Ics.Fmx.OverbyteIcsThreadTimer.pas
OverbyteIcsWebSocketCli            -> Ics.Fmx.OverbyteIcsWebSocketCli
OverbyteIcsWebSocketSrv            -> Ics.Fmx.OverbyteIcsWebSocketSrv
OverbyteIcsWSocket                 -> Ics.Fmx.OverbyteIcsWSocket
OverbyteIcsWSocketS                -> Ics.Fmx.OverbyteIcsWSocketS
OverbyteIcsWhoisCli                -> Ics.Fmx.OverbyteIcsWhoisCli.pas
OverbyteIcsWndControl              -> Ics.Fmx.OverbyteIcsWndControl

{ Demo units }
OverbyteIcsWebAppServerCounter     -> Ics.Fmx.OverbyteIcsWebAppServerCounter
OverbyteIcsWebAppServerMailer      -> Ics.Fmx.OverbyteIcsWebAppServerMailer

The list above is also the list of units that now have different names in the FireMonkey
framework however most of them share the same source file.

Dropping a ICS component on the form will add the correct unit name for each framework
automatically.


Delphi Windows sample applications:
-----------------------------------

ICS contains numerous sample applications developed over 25 years, they are split into
directories by platform, VCL and FMX.  Unlike earlier ICS releases what had separate
Win64 project groups, all these samples now build into separate platform/config directories
depending on project settings, all can be built for Win64.

You may encounter an error loading a sample application or running it on older versions of
Delphi, because they are saved with the more recent version of Delphi that may have extra
properties over older versions.  You can safely ignore messages related to those new
properties, which are not used in the samples. You can also encounter error about duplicate
resources with older samples which you can ignore, all the samples in the vcl group should
build cleanly without errors or warnings.


.\demos-delphi-vcl
----------------

This directory contains most of the actively maintained demos used to develop and test
ICS for Windows VCL applications, the project groups are:

.\demos-delphi-vcl\demos-delphi-vcl.groupproj - modern compilers, Delphi XE2 and later
.\demos-delphi-vcl\demos-delphi-vcl-legacy.bpg - legacy compilers, Delphi 7 and later
.\demos-delphi-vcl\demos-delphi-vcl-legacy.groupproj - legacy compilers, Delphi 2007 and later

Beware the sample project files (.dproj) supplied are built with modern compilers,
and can not be opened by legacy compilers due to new platforms and features.  So
for Delphi XE and earlier (and maybe some other XE versions), before opening a
group or application project, you MUST delete all .dproj sample files.  When you
open the project, the .dproj file will be automatically recreated from the .dpr
project file by Delphi.  If you attempt to open a new .dproj file with a legacy
Delphi compiler, it will simply give an XML error and not attempt to rebuild the
project file.

ICS VCL Client Samples
OverbyteIcsSnippets          Small samples of codes for FTP, HTTP, sockets and email
OverbyteIcsHttpRestTst       HTTPS web, REST and OAuth, WebSocket, Send SMS, Rest Email, and DoH demos.
OverbyteIcsHttpThrd          Threaded HTTPS using TSslHttpRest.
OverbyteIcsHttpsTst          HTTPS GET using TSslHttpCli component, tests all commands
OverbyteIcsIpStmLogTst       IP stream logging, sending streams as client or server using TIcsIpStrmLog and IcsHosts
OverbyteIcsMailQuTst         Mailing list tool using TIcsMailQueue component with SMTP to send email
OverbyteIcsSslFtpTst         FTP client with SSL/TLS, using TSslFtpClient component, tests all commands
OverbyteIcsSslMailRcv        POP3 receive email client using TSslPop3Cli component, tests all commands
OverbyteIcsSslMailSnd        SMTP send email client using TSslSmtpCli component, tests all commands
OverbyteIcsSslNewsRdr        NNTP Network News Transport client, using TSslNntpCli, tests all commands
OverbyteIcsXferTst           File copying, FTP upload and download, HTTP download, using TIcsFileCopy, TIcsFtpMulti and TIcsHttpMulti
OverbyteIcsConHttp           Shows how to use TSslHttpRest component within a console mode application.

ICS VCL Server Samples
OverbyteIcsIpStmLogTst       IP stream logging, sending streams as client or server using TIcsIpStrmLog and IcsHosts
OverbyteIcsProxySslServer    Proxy server for TCP and HTTP protocols using TIcsProxy and IcsHosts
OverbyteIcsSslFtpServ        FTP server with SSL/TLS, using TSslFtpServer component
OverbyteIcsSslMultiFtpServ   Multi host FTP server, using TSslFtpServer and IcsHosts components
OverbyteIcsSslMultiWebServ   Multi host HTTPS web and WebSocket server, uses TSslHttpAppSrv and IcsHosts components
OverbyteIcsSslSmtpServer     SMTP receive email server using TSslSmtpServer component
OverbyteIcsSslWebServ        HTTPS web server, uses TSslHttpServer component
OverbyteIcsWebAppServer      HTTPS web server with sessions, uses THttpAppSrv component
OverbyteIcsBasicWebServer    Simplified version of OverbyteIcsSslMultiWebServ.

ICS VCL Tool Samples
OverbyteIcsBatchDnsLookup    Multiple DNS lookups using TIcsDomainNameCache component
OverbyteIcsJoseTst           SSL Json Object Signing (Jose) Demos, used for REST and OAUTH2
OverbyteIcsNetMon            Internet Packet Monitoring Components, using TIcsMonSocket and TIcsMonPCap
OverbyteIcsNetTools          Network Tools Demo, uses all the main IP Helper functions and many other components
OverbyteIcsNsLookup          DNS lookups using the TDnsQuery component, all request types.
OverbyteIcsPemTool           SSL/TLS Certificate Tool, process certificates using TX509Base and TX509List
OverbyteIcsPingTst           Trace route and pinging using the TPing and TPingThread components
OverbyteIcsWhoisCliTst       Whois client, looks up servers automatically, using TIcsWhoisCli component
OverbyteIcsWmiTst            WMI functions, general purpose, update Windows IP addresses and DNS Server records
OverbyteIcsX509CertsTst      Download SSL X509 certificates from Let's Encrypt using TSslX509Certs component

ICS VCL Miscellaneous Samples
OverbyteIcsBinCliDemo        Simple TCP client to receive binary and text data for OverbyteIcsTcpSrv
OverbyteIcsCliDemo           Simple TCP client for OverbyteIcsTcpSrv, IPV4 only
OverbyteIcsHttpPost          HTTP web POST using THttpCli component, works with all HTTP server samples
OverbyteIcsHttpTst           HTTP web GET using THttpCli component, tests all commands
OverbyteIcsMimeDemo          MIME email decoding, attached files are extracted using TMimeDecodeW
OverbyteIcsSimpleSslCli      Simple SSL TCP client using TSslWSocket component
OverbyteIcsSimpleSslServer   Simple SSL TCP server using TSslWSocket component
OverbyteIcsSocksTst          SOCKS and HTTP tunnel proxy client testing using TSslWSocket component
OverbyteIcsSnmpCliTst        SNMP (simple network management protocol) using TSnmpCli component
OverbyteIcsSysLogClientDemo  SysLog client using TSysLogClient component
OverbyteIcsSysLogServerDemo  SysLog server using TSysLogServer component
OverbyteIcsTcpSrv            Basic TCP server without client forms, uses TWSocketServer, IPv4 only
OverbyteIcsTcpSrvIPv6        Basic TCP server without client forms, uses TWSocketServer, IPv4/IPV6
OverbyteIcsTelnetClient      Telnet terminal client using TnEmulVT component
OverbyteIcsThrdSrv           Multithreaded TCP server, banner sent in main thread, uses TWSocketServer component
OverbyteIcsThrdSrvV2         Multithreaded TCP server, banner sent in worker thread, uses TWSocketServer component
OverbyteIcsThrdSrvV3         Multithreaded TCP server, uses TWSocketThrdServer component
OverbyteIcsTimeTst           SNTP time protocol client and server, using TIcsTimeClient and TIcsTimeServer components


.\demos-delphi-extra
--------------------

.\demos-delphi-extra\demos-delphi-extra.groupproj

VCL samples that need third party components to build.  FrameBrowserIcs requires the HtmlViewer
component installed from https://github.com/BerndGabriel/HtmlViewer
OverbyteIcsDDWebService and IcsAppMon require DDService framework to be installed from
https://www.magsys.co.uk/delphi/ddservice.asp
OverbyteIcsMQTTst needs the VirtualTree component to be installed from GetIt or
https://github.com/TurboPack/VirtualTreeView

FrameBrowserIcs              Web Browser using TSslHttpCli and HtmlViewer component
OverbyteIcsMQTTst            MQ Telemetry Transport message queuing service
OverbyteIcsDDWebService      Multi host HTTPS web and WebSocket server, uses TSslHttpAppSrv and IcsHosts components, maybe run as a GUI or Windows Service
IcsAppMon                    ICS Application Monitor server monitors apps using TIcsAppMonCli and ensures they remain running.
IcsAppMonMan                 ICS Application Monitor - Remote Manager provides remote monitoring of multiple IcsAppMon servers using Json web and websocket requests.


.\demos-delphi-fmx
------------------

.\demos-delphi-fmx\demos-delphi-fmx.groupproj

FMX samples for Windows, not yet tested on MacOS
IcsPemTest                   FMX limied PEM function tests for Posix.
IcsHttpRestTstFmx            FMX HTTPS web, REST and OAuth, Send SMS, Rest Email, and DoH demos
IcsHttpsTst                  FMX HTTP web GET using THttpCli component, tests all commands
IcsSslMultiWebServ           FMX Multi host HTTPS web server, uses TSslHttpAppSrv and IcsHosts components
IcsCliDemo                   Example of client for SRVDEMO, IPV4 only
IcsTcpSrvIPv6                Basic server without client forms, event-driven, IPv4/IPV6
IcsMailSnd                   Example of EMail sending using SMTP, including file attach
IcsBatchDnsLookup            Batch async DNS lookup using DnsLookup (IPv6 and IPv4)


.\demos-cpp-vcl
---------------

Old C++ samples that have not been tested for 10 years



Pre-Built Samples
-----------------

All the main ICS active samples are available as prebuilt executables, to
allow ease of testing without needing to install ICS and build them all.
They are available to download from the wiki pages:

https://wiki.overbyte.eu/wiki/index.php/Main_Page

as four separate zip files split into clients, servers, tools and miscellaneous
samples.  All are built with Delphi 11.3 and those SSL/TLS are built with
OpenSSL embedded to needing external DLLs to be distributed.  The zips include a
few extra SSL certificate and other files to support the samples.

These samples should not treated as commercial applications, they are merely to
illustrate the type of applications the ICS components can be used to create.


ICS Code Snippets
-----------------

The SSL sample OverbyteIcsSnippets contains small samples of codes for FTP,
HTTP, sockets and email.  The unit includes several almost self contained
methods each implementing a single functions, which are hopefully easier to
follow than the normal samples used to develop ICS components and which often
become very complicated due to all the different functionality supported. The
snippets are heavily documented to try and explain usage.

Most of the snippets access Magenta Systems Ltd public ICS web and FTP servers
and should just work without change, except for FTP uploading where you will
need to request an account by emailing delphi@magsys.co.uk.  Snippets available
include:

Snippet: View Local Directories - print a directory file listing.
Snippet: File Copy One File - copy a single file.
Snippet: File Copy Multiple Files - copy multiple files.
Snippet: FTP View Directories - print a remote directory listing from an FTP site.
Snippet: FTP Download One File - download a single file from an FTP site.
Snippet: FTP Download Multiple Files - downloads multiple files from an FTP site.
Snippet: FTP Upload One File - upload a single file to an FTP site.
Snippet: FTP Upload Multiple Files - upload multiple files to an FTP site.
Snippet: HTTP Download List of Files - downloads a list of files from a web site.
Snippet: HTTP Download Linked Files - downloads multiple files from a web site by
          parsing HTML pages for links.
Snippet: HTTP REST Json Request - makes an HTTP GET request to a REST server
          receiving a Json response data.
Snippet: HTTP REST Download - makes an HTTP GET request to download a file, with
         optional resume of partial download.
Snippet: HTTP POST Upload File - makes a HTTP POST request to upload a file to a
         special upload web page.
Snippet: Local Socket Traffic - Send simple text traffic between two sockets on
         the same PC, using client server concepts.
Snippet: Remote Socket Traffic - Receive simple text traffic from a remote TCP
         Server.
Snippet: WebSocket Client - Connect to a remote WebSocket server to send and
         receive data.
Snippet: Send Email using Mail Queue - Runs a mail queue to send multiple
         emails with extended retries over many hours or days.


Older Project Groups and Samples
--------------------------------

There are many older samples, mostly non-SSL, but some designed to test or illustrate
specific components or programming techniques such as console applications. These
are no longer included in the main ICS distribution but are available as a separate
downloaded from the ICS wiki pages.

https://wiki.overbyte.eu/wiki/index.php/Main_Page

Note while these samples all build with V9.1 and later, few have been tested for many
years and they won't include many features added to ICS during that time.

.\Samples\delphi\OldDemosProject.groupproj

Note this group and some samples were last saved with a modern Delphi compiler, so
may not open with legacy compilers, try deleting the .dproj file and then reopen.


Delphi Win32/Win64 FTP sample non-SSL applications
--------------------------------------------------
> OverbyteIcsBasFtp          Basic FTP client program
> OverbyteIcsConFtp          Basic console mode FTP client
> OverbyteIcsFtpAsy          Example of asynchronous FTP client
> OverbyteIcsFtpMulti        Demo to do several FTP downloads in parallel to get a list of files
> OverbyteIcsFtpMultipartDownload  Demo to FTP download a single large file in several parts in parallel
> OverbyteIcsFtpServ         General purpose FTP server, uses TSocketServer
> OverbyteIcsFtpThrd         Demo of multithreaded FTP client, see also FTPASY
> OverbyteIcsFtpTst          Basic graphical FTP client

Delphi Win32/Win64 SMTP, POP3, NNTP non-SSL sample applications
---------------------------------------------------------------
> OverbyteIcsBasNntp         Basic NNTP client program
> OverbyteIcsConPop3         Basic console mode demo for POP3 (mail receive)
> OverbyteIcsConSmtp         Basic console mode demo for SMTP (mail send)
> OverbyteIcsMailHtml        Example of HTML formatted EMail sending, including embedded images
> OverbyteIcsMailRcv         Internet EMail access using POP3 protocol
> OverbyteIcsMailSnd         Example of EMail sending using SMTP, including file attach
> OverbyteIcsMailSndAsync    Example of parallel EMail sending with multiple connections
> OverbyteIcsNewsReader      Example of TNntpCli component (Send/receive newsgroups)
> OverbyteIcsSmtpServer      Internet EMail server using SMTP protocol

Delphi Win32/Win64 Miscellaneous applications
---------------------------------------------
> OverbyteIcsBufStrmTst      Test of buffered stream classes
> OverbyteIcsCacheTest       Test of TCacheTree class used in TSslAvlSessionCache
> OverbyteIcsMD4Test         Test program for MD4 unit
> OverbyteIcsMD5File         Example of MD5 unit: computer MD5 checksum for files
> OverbyteIcsMD5Test         Test program for MD5 unit
> OverbyteIcsOneTimePassword One Time Password test routines for OverByteIcsOneTimePw unit
> OverbyteIcsSHA1Test        Test program for SHA unit
> OverbyteIcsThreadTimerDemo Demo for TIcsThreadTimer
> OverbyteIcsTicks64Demo     GetTickCount64 test routines for OverbyteIcsTicks64 unit
> OverbyteIcsTimerDemo       Very simple demo for TIcsTimer
> OverByteIcsWndControlTest  Test program for windows and threads

Delphi Win32/Win64 DNS, Ping, SNMP, Syslog sample applications
--------------------------------------------------------------
> OverbyteIcsConPing        Basic console mode demo for ping component
> OverbyteIcsDll1           Demo showing how to use a TWSocket component in a DLL
> OverbyteIcsDll2           Demo showing how to use a THttpCli component in a DLL
> OverbyteIcsDllTst         Test program calling ICSDLL1 and ICSDLL2
> OverbyteIcsDnsLook        Example of name resolution (IPv6 and IPv4)
> OverbyteIcsDnsResolver    Batch async DNS lookup event driven using DnsQuery
> OverbyteIcsFinger         Example of TFingerCli component

Delphi FireMonkey cross-platform samples (Delphi XE2 and later)
--------------------------------------------------------------
.\Samples\delphi\PlatformDemos\XSamples.groupproj
> IcsConSmtp            Basic console mode demo for SMTP (mail send)
> IcsMailRcv            Internet EMail access using POP3 protocol
> IcsWebServ            Demo of HTTP server, uses TSocketServer
> IcsWebAppServ         Advanced HTTP server demo, uses WebServ, adds sessions
> IcsFtpTst             Basic graphical FTP client
> IcsFtpServ            General purpose FTP server, uses TSocketServer
> IcsUdpLstn            UDP listen demo
> IcsUdpSend            UDP send demo
> IcsDll1               Demo showing how to use a TWSocket component in a DLL
> IcsDll2               Demo showing how to use a THttpCli component in a DLL
> IcsDllTst             Test program calling ICSDLL1 and ICSDLL2
> IcsThreadTimerDemo    Very simple demo for TIcsTimer

 Delphi Win32/Win64 Socket sample applications
--------------------------------------------------------------
> OverbyteIcsClient5       Basic client GUI applications
> OverbyteIcsClient7       Simple client application demonstrating TWSocket
> OverbyteIcsConCli1       Basic client/server console applications
> OverbyteIcsConCli2       Basic client/server console applications with thread
> OverbyteIcsConSrv1       Basic server application in console mode
> OverbyteIcsConUdpLstn    Console application to listen for UDP messages
> OverbyteIcsDynCli        Demo of dynamically created TWSocket components
> OverbyteIcsMtSrv         Basic server, multi-threaded, see THRDSRV for better code
> OverbyteIcsRecv          Simple file receive (server), use with SENDER demo (client)
> OverbyteIcsSender        Simple file send (client), use with RECV demo (server)
> OverbyteIcsServer5       Basic server GUI applications
> OverbyteIcsSrvTcp        Basic server without client forms, event-driven
> OverbyteIcsSvcTcp        Same as SRVTCP but as an NT/2K/XP service
> OverbyteIcsTWSChat       Chat program (both client and server in a single program)
> OverbyteIcsTnDemo        Telnet client using a TMemo
> OverbyteIcsTnSrv         Basic TCP server with client forms, event-driven
> OverbyteIcsUdpLstn       UDP listen demo
> OverbyteIcsUdpSend       UDP send demo


Delphi Win32/Win64 SSL-enabled sample applications
--------------------------------------------------
> OverbyteIcsMsVerify          Verify certificate chain using TMsCertChainEngine which uses MS crypto API
> OverbyteIcsSslSniSrv         Test of Server Name Indication (SNI) in server mode
> OverbyteIcsSslWebAppServer   Advanced HTTPS server demo, uses WebServ, adds sessions
> OverbyteIcsWebSocketSrv      Demo of WebSockets server - old version, now OverbyteIcsSslMultiWebServ
> websocketclient.html         Web page for WebSockets demo


Delphi Win32/Win64 HTTP sample applications
-------------------------------------------
> OverbyteIcsConHttp           Basic console mode HTTP client
> OverbyteIcsHttpAsp           Example of THttpCli component with cookie (POST to an ASP page)
> OverbyteIcsHttpAsy           Example of THttpCli component with multiple async requests (GET)
> OverbyteIcsHttpChk           Example of THttpCli to check for valid URL using HEAD request
> OverbyteIcsHttpDmo           Simple HTTP client demo with proxy
> OverbyteIcsHttpGet           Example of THttpCli component (GET into a file)
> OverbyteIcsHttpMultipartDownload    Demo application for TMultipartHttpDownloader
> OverbyteIcsHttpPg            Example of THttpCli component (POST to CGI script)
> OverbyteIcsHttpThrd          Example of THttpCli component (multi-threaded GET)
> OverbyteIcsIsapi.dll         Example of FTP client component within an HTTP server ISAPI extension
> OverbyteIcsWebServ           Demo of HTTP server, uses TSocketServer



Getting Started with ICS
------------------------

ICS has a large number of sample application whose primary purpose is to test
all the components and to learn about using those components and how to use
them in your own applications.  There are often several samples for a single
protocol with different purposes, so this section should help get you started
choosing the components and samples for your internet project.

ICS often offers low and high level versions of components, the former allow
your application to send the various commands used by the protocol but you
need to send those commands in the correct order often dependent upon the
result from earlier commands, so you need to understand the protocol, but
have control over the commands.  The high level components are quicker and
easier to implement because they hide most of the protocol and offer complex
methods instead such as download a file, they often include extra functionality.

Historically, most ICS components are available on non-SSL and SSL versions,
these notes assume you are using SSL/TLS components which are often essential
today.  Note most low level component need SSL/TLS adding using an SslContext
and need SSL certificate chain checking added to applications, while the
higher level components mostly already include the SslContext and chain
checking and hide much of the SSL/TLS complexity making them faster to
implement and easier to maintain as SSL changes.


World Wide Web, HTTP Client
---------------------------

There are four types of HTTP component, with many extra components used to
extend their capabilities.

TSslHttpCli in unit OverbyteIcsHttpProt is the low level HTTP protocol client
that is tested using sample OverbyteIcsHttpsTst. It has buttons for GET and
HEAD commands and allows numerous SSL parameters to be specified. POST requests
are tested with samples OverbyteIcsHttpPost and OverbyteIcsHttpPg. Other units
containing components assisting HTTP include OverbyteIcsHttpCCodZLib,
OverbyteIcsHttpContCod, OverbyteIcsCookies, OverbyteIcsMimeUtils,
OverbyteIcsFormDataDecoder, OverbyteIcsCharsetUtils, OverbyteIcsMsSslUtils,
MIME with sample OverbyteIcsMimeDemo, SSL certificate chains with sample
OverbyteIcsMsVerify. Note TSslHttpCli requires an SslContext for SSL
configuration.  Note HTTP clients do not need SSL/TLS certificates, but
generally should check the certificate chains received from HTTPS servers
to ensure they are talking to the correct servers.

TSslHttpRest in unit OverbyteIcsSslHttpRest is the high level HTTP protocol
client that has additional methods and properties for making GET, POST, PUT
and HEAD REST (REpresentional State Transfer) client requests, but can
still do everything TSslHttpCli does.  It includes a TRestParams class to
build and encode GET/PUT/POST parameter strings. It also includes SSL
configuration and certificate validation with a root bundle, SSL session
caching, content compression, content code page decoding, persistent
cookies, Json handling, logging and client SSL certificate support.  There
some REST examples TDnsQueryHttps, TIcsSms, TIcsTwitter and TIcsRestEmail.
All tested using sample OverbyteIcsHttpRestTst.

TRestOAuth in unit OverbyteIcsSslHttpOAuth handles OAuth1/2 authentication
using either embedded EdgeBrowser or TWebBrowser to display the logins web
pages, or they can be viewed using an external browser.  The unit supports
various Microsoft User Authorities for corporate accounts. Note OAuth1/2 use
requires a developer application account at Google or Microsoft, or other
providers which includes Ids and secrets that need to be securely stored.

TIcsHttpMulti in unit OverbyteIcsHttpMulti is another high level HTTP client
that allows downloading of multiple files from an HTTP server using full URLs,
or listed by parsing links from a web page, using a single function call. It
also includes SSL configuration and certificate validation with a root bundle.
Tested using sample OverbyteIcsXferTst.

TSslWebSocketCli in unit OverbyteIcsWebSocketCli is WebSocket client component
that descends from TSslHttpRest so most of it's properties and events are common,
but there are new methods and events to access WebSocket servers using ws:// or
wss:// URLs.  WebSocket is a full duplex TCP protocol for web servers to support
interactive web pages, typically dynamic updating such as chat sessions, spell
checkers as you type, search hints, etc. Sample IcsAppMonMan uses WebSockets to
access WebSocket servers in IcsAppMon servers.

ICS has a visual web browser sample FrameBrowserIcs which needs the HtmlViewer
component to be installed, which will view simple web pages that don't need
Javascript, it logs both HTTP and HTML protocol and can be very useful for
debugging.

There are two SSL samples OverbyteIcsHttpsTst and OverbyteIcsHttpRestTst
that illustrate HTTP GET and POST requests, authentication including OAuth2,
file uploading and downloading, cookies, certificate chain verification,
content encoding and decoding and WebSockets with a chat demo.

For console applications, OverbyteIcsConHttp makes a simple HTTPS rwquest.

There are some older non-SSL demos for console and DLL and threads, see
samples OverbyteIcsHttpAsp and OverbyteIcsHttpThrd. Another sample
OverbyteIcsJoseTst can be used to test Json Object Signing (Jose) functions
often used for REST requests, URL encoding and decoding and display of Json
and XML data.


World Wide Web, HTTP Server
---------------------------

There are five different HTTP web servers, which are based on
TSslWSocketServer.

TSslHttpServer in unit OverbyteIcsHttpSrv is the main web server, tested
with sample OverbyteIcsSslWebServr, while TSslHttpAppSrv in unit
OverbyteIcsHttpAppServer adds session support and page handlers for creating
dynamic page web applications tested with sample OverbyteIcsSslWebAppServer.
These servers only listen on one IP address and port, but you use multiple
components for multiple listeners sharing the same events.  Note TSslHttpServer
and TSslHttpAppSrv require an SslContext for SSL configuration. The samples
are full web servers with a lot of SSL configuration options for an SSL/TLS
certificate, note HTTPS servers require an SSL certificate and will not start
without one. Both samples include a number of dynamic web pages to illustrate
basic web server facilities, including a contact form that sends email.

The WebSockets protocol is supported using the THttpWSSrvConn client class
instead of THttpAppSrvConnection for only normal HTTP.

There is a third more advanced HTTP sample OverbyteIcsSslMultiWebServ
which configures TSslHttpAppSrv differently using collections of
IcsHosts properties. This allows the web server to listen on multiple
IP addresses and ports at the same time, and to support multiple hosts
on the same IP address serving different page content (as do most
web servers).  IcsHosts allow different SSL/TLS certificates to be
specified for each host using built-in SslContexts, will automatically
create ICS signed SSL/TLS certificates so the server can start, and will
them order free SSL/TLS certificates from Let's Encrypt (provided running
on the public internet), and re-order them every three months before they
expire.  IcsHosts can accept server certificates as PEM or PFX files, or
from the Windows Certificate Store (but not from USB dongles). The sample
OverbyteIcsSslMultiWebServr is different to most ICS samples in having a
minimal GUI and being entirely configured using an INI file, it is really
designed to be built as a Windows service application to run unattended
in background.  It includes a separate web log for each host, and will
send emails when it starts and stops.  It also includes some anti-hacking
tests and will block abusive IP addresses. The sample is based on a
commercial web server.

Sample OverbyteIcsSslMultiWebServ also includes WebSockets support with
simple echo servers and a chat server.

Sample OverbyteIcsBasicWebServer is a simplified OverbyteIcsSslMultiWebServ
ignoring configuration INI files, security features, session data, most demo
pages and most logging, and settings for localhost set in code, search for
IcsHosts to change IP addresses, etc. But much easier to get started if
creating a new server.

Sample OverbyteIcsDDWebService is very similar to OverbyteIcsSslMultiWebServ
but designed as a Windows service, although it will also run as a GUI for
debugging.  It requires DDService service framework to be installed. It also
includes a REST server with simple lookup responses from a database.

The fifth web server is TSimpleWebSrv in unit OverbyteIcsSslHttpOAuth which
is a lightweight server with minimal functionality designed for embedding
in applications needing OAuth2 or SSL/TLS certificate ordering that require
access to web server to check a host exists.  It has a single event that
presents a request and returns a response. It supports SSL with IcsHosts.
There is no sample, but it is used by other ICS components.



File Transfer Protocol, FTP Client
----------------------------------

There are two types of FTP components for file transfers.

TSslFtpClient in unit OverbyteIcsFtpCli is the low level FTP client that is
tested with sample OverbyteIcsSslFtpTst.  It has about 50 buttons the test the
various FTP commands in various ways, and allows numerous SSL parameters to
be specified. Note TSslFtpClient requires an SslContext for SSL configuration.
Other older FTP samples include OverbyteIcsBasFtp, OverbyteIcsConFtp,
OverbyteIcsFtpAsy and OverbyteIcsFtpMulti.

TIcsFtpMulti in unit OverbyteIcsFtpMulti is a high level FTP client that indexes,
uploads or downloads single or multiple files automatically, without needing
to understand most FTP commands.  One function indexes files and directories
on an FTP server building a list compatible with the TIcsFileCopy component
that indexes Windows directories, allowing local and remote directories to
be compared and files FTP uploaded or downloaded so they match.  It also
includes SSL configuration and certificate validation with a root bundle,
SSL session caching and logging.  Use the sample OverbyteIcsXferTst to test
TIcsFtpMulti.


File Transfer Protocol, FTP Server
----------------------------------

The FTP server is based on TSslWSocketServer.

TSslFtpServer in unit OverbyteIcsFtpSrv is the FTP server, tested using
sample OverbyteIcsSslFtpServ. The FTP server only listens on one IP address
and port, but you use multiple components for multiple listeners sharing the
same events.  Note TSslFtpServer usually requires an SslContext for SSL
configuration. The sample is a full FTP server for file uploads and
downloads, with a lot of SSL configuration options for the SSL/TLS
certificate and will not start without one.

There is a more advanced FTP server sample OverbyteIcsSslMultiFtpServ
which configures TSslFtpServer differently using collections of
IcsHosts properties. This allows the FTP server to listen on multiple
IP addresses and ports at the same time, and to support multiple hosts
on the same IP address.  IcsHosts allow different SSL/TLS certificates to
be specified for each host using built-in SslContexts, will automatically
create self signed SSL/TLS certificates so the server can start, and will
them order free SSL/TLS certificates from Let's Encrypt (provided running
on the public internet), and re-order them every three months before they
expire. OverbyteIcsSslMultiFtpServ is different to most ICS samples
in having a minimal GUI and being entirely configured using an INI file,
it is really designed to be built as a Windows service application to run
unattended in background. The sample is based on a commercial FTP server.


Sending Email, SMTP Client
--------------------------

There are three types of components for sending email using the SMTP protocol
or HTTP REST protocol.

TSslSmtpCli in unit OverbyteIcsSmtpProt is the low level SMTP client that
is tested with sample OverbyteIcsSslMailSnd1.  It has about 16 buttons to
test various SMTP commands and allow an email to be sent with attachments.
Note TSslSmtpCli requires an SslContext for SSL configuration. Other older
test samples include OverbyteIcsConSmtp, OverbyteIcsMailHtml and
OverbyteIcsMailSndAsync.

TIcsMailQueue in unit OverbyteIcsMailQueue is the high level SMTP client,
tested by sample OverbyteIcsMailQuTst.  It supports extended retries over many
hours or days, and supports multiple SMTP relay servers or looks up MX servers
using DNS, while alleviating the need for the application to handle retries.
It spools emails as EML files, and can send them as well.  It includes SSL
configuration and certificate validation with a root bundle and logging.

TIcsRestEmail in unit OverbyteIcsSslHttpRest is alternative means of
sending email using HTTP REST requests to Google and Microsoft, instead
of using SMTP. Tested using sample OverbyteIcsHttpRestTst.  This component
also adds XOAuth2 authentication to the other SMTP components.

All the mail components can use TRestOAuth in unit OverbyteIcsSslHttpOAuth
to handle OAuth2 authentication using either embedded EdgeBrowser or
TWebBrowser to display the logins web pages, or they can be viewed using an
external browser.  The unit supports various Microsoft User Authorities for
corporate accounts.  Note OAuth2 use requires a developer application account
at Google or Microsoft, or other providers which includes Ids and secrets that
need to be securely stored.

While mailboxes requiring OAuth2 require an initial interactive login, once
that completes successfully a refresh token is returned which can be securely
stored and treated like a password  for future access without requiring another
login.  The refresh token can also be used by other applications provided they
uses the same account Ids and secrets, allowing non-interactive applications
like the ICS web, FTP and proxy servers to use GMail using TIcsMailQueue.


Receiving Email, POP3 Client
----------------------------

There are two types of components for receiving email using the POP3 protocol
or HTTP REST protocol.

TSslPop3Cli in unit OverbyteIcsPop3Prot is the low level POP3 client that
is tested with sample OverbyteIcsSslMailSnd1.  It has about 22 buttons to
test various POP3 commands and allow emails to be retrieved from a mailbox.
The unit OverbyteIcsMimeDec contains functions for decoding MIME encoded
emails, tested with sample OverbyteIcsMimeDemo. AnOther older test sample
is OverbyteIcsConPop3.  Note TSslPop3Cli requires an SslContext for SSL
configuration.

TIcsRestEmail in unit OverbyteIcsSslHttpRest is alternative means of
receiving email using HTTP REST requests to Google and Microsoft, instead
of using POP3. Tested using sample OverbyteIcsHttpRestTst.  This component
also adds XOAuth2 authentication to the POP3 component.  TRestOAuth in
unit OverbyteIcsSslHttpOAuth handles OAuth2 authentication, see above.


Forwarding Email, SMTP Server
-----------------------------

TSslSmtpServer in unit OverbyteIcsSmtpSrv is an SMTP server that accepts
emails from a client, making some checks and adding headers, which is
tested by sample OverbyteIcsSslSmtpServ which writes emails to an EML spool
file.  Note neither component or sample support POP3 access, nor do they
do anything with the EML file.  The TIcsMailQueue component could be
used to forward EML files.  Note TSslSmtpServer requires an SslContext
for SSL configuration and SSL/TLS certificate, it does not yet support
IcsHosts.


Simple TCP Socket Client
------------------------

TSslWSocket in unit OverbyteIcsWSocket is the root of most other ICS
components opening a socket to either connect to a remote server, or to
listen for connections from a remote server.  The component always opens
a socket by IP address, but will look-up that IP address from a host
name if required, or provide a reverse look-up of host or domain name
from an IP address. TSslWSocket sends or receives a stream of 8-bit
binary characters, but does have methods to send and receive lines by
checking or sending a CRLF line ending, which is the Telnet protocol,
used for the headers all most other high level protocols like HTTP,
FTP, SMTP, etc.  TSslWSocket can use TCP or UDP transmission, most
protocols use TCP, except DNS and SNMP. TSslWSocket can be tested using
samples OverbyteIcsSimpleSslCli, OverbyteIcsCliDemo, OverbyteIcsBinCliDemo,
OverbyteIcsUdpLstn, OverbyteIcsUdpSend and many others.  Note TSslWSocket
requires an SslContext for SSL configuration.

TIcsIpStrmLog in unit OverbyteIcsIpStreamLog is a higher level version
of TSslWSocket, originally designed for IP stream logging with minimal
events and extra coding, including an SslContext and full SSL/TLS
certificate chain checking, with better line handling, multiple
connection attempts and retries on failure or loss of connection.
TIcsIpStrmLog can be configured a client or server, TCP or UDP, and
is tested by sample OverbyteIcsIpStmLogTst which can run as client and
server at the same time, sending data to itself.


Simple TCP Socket Server
------------------------

TSslWSocketServer in unit OverbyteIcsWSocketS is the main socket server
accepting a few thousand remote clients using multiple IP addresses and
ports, and separately allowing data to be sent and received from those
remote clients, all in a single thread.  Applications need to derive
a client from TSslWSocketClient into which the required functionality
is added.  TSslWSocketServer supports using collections of IcsHosts
properties. This allows the server to listen on multiple IP addresses
and ports at the same time with different SSL/TLS certificates for each
host using built-in SslContexts, will automatically create self signed
SSL/TLS certificates so the server can start, and will them order free
SSL/TLS certificates from Let's Encrypt (provided running on the public
internet), and re-order them every three months before they expire.

TSslWSocketServer is mostly tested using the ICS HTTP and FTP servers,
but there are other samples, OverbyteIcsSimpleSslServer, OverbyteIcsTcpSrv,
OverbyteIcsTcpSrvIPV6, OverbyteIcsThrdSrv, OverbyteIcsThrdSrvV2, etc.

TIcsIpStrmLog mentioned just above uses TSslWSocketServer for simpler
server applications with a small number of remote clients.

There is also a threaded version TSslWSocketThrdServer in unit
OverbyteIcsWSocketTS where each client is created with a separate thread
to avoid blocking on high load servers.  Beware this server does not yet
support IcsHosts and multiple IP addresses, nor is there a web server
using it.  It is tested using sample OverbyteIcsThrdSrvV3.


Forward or Reverse Proxy Server
-------------------------------

TIcsProxy and TIcsHttpProxy in unit OverbyteIcsProxy are designed
for forward or reverse socket proxying and are tested by sample
OverbyteIcsProxySslServer.  Despite the component names, these
components support SSL using IcsHosts with all the usual functions.
TIcsProxy is protocol agnostic and may be used to proxy any TCP protocol,
the sample includes SMTP, POP3, NNTP and telnet. TIcsHttpProxy is a full
forward and reverse HTTP/HTTPS proxy with header and body parsing and
processing host names and URLs to match the source and destination.
Note the sample has a minimal GUI and is configuring using an INI file.


Websockets Client and Server
-----------------------------

See HTTP Client and HTTP Server above.


MQ Telemetry Transport
----------------------

TIcsMQTTServer and TIcsMQTTClient in OverbyteIcsMQTT handle the MQ Telemetry
Transport message queuing service, tested by sample OverbyteIcsMQTTst which
has both client and server,


Telnet Client
--------------

TTnCnx in unit OverbyteIcsTnCnx implements the TCP/IP telnet protocol
including some options negotiations, tested by sample OverbyteIcsTnDemo.
TTnEmulVT in unit OverbyteIcsTnEmulVT offers ANSI terminal emulation
tested using sample OverbyteIcsTelnetClient.


Network News Reader, NNTP Client
--------------------------------

TSslNntpCli in unit OverbyteIcsNntpCli is a NNTP client, tested by
sample OverbyteIcsSslNewsRdr with 28 buttons for the various commands.


Create, Order or Review SSL/TLS Certificates
--------------------------------------------

ICS contains many functions for processing SSL/TLS X509 certificates and
private keys. TX509Base in unit OverbyteIcsWSocket may contain and server
or client certificate, private key and one of more intermediate
certificates, and has properties to display most of the certificate
elements, all tested by sample OverbyteIcsPemtool.  TX509List contains
multiple certificates, typically a root store loaded from a PEM file.
TMsX509List will load certificates from any Windows Certificate Store
including private keys. TSslCertTools in unit OverbyteIcsSslX509Utils can
read, create and save certificates, private keys, certificate requests and
sign requests as a certificate authority.

The OverbyteIcsPemtool sample can be used to create certificates and
private keys in various RSA, EC and other formats, create certificate
requests and sign requests as a certificate authority, and convert
certificate files between different formats, by reading as one and saving
as another, also combining keys and certificates in a file.
Root stores and single certificates may be to be viewed.  Certificates
and private keys in Windows Stores may be viewed and deleted, and a PEM
or PFX certificate bundle installed into any Windows Store.

TSslX509Certs in unit OverbyteIcsSslX509Certs, tested by sample
OverbyteIcsX509CertsTst which automatically downloads SSL/TLS X509
certificates from various issuers, including free certificates from Let's
Encrypt, and  commercial certificates from CertCentre AG. Supports ACME
V2 protocol, and REST protocols for specific vendors.  Domain and DNS
validated certificates should generally  be issued without intervention,
other commercial certificates may take days to be approved. This unit may
be added to ICS server applications using IcsHosts for automatic ordering,
while the sample may be separately used to order certificates manually,
including DNS validated wildcard certificates from Let'S Encrypt.  All orders
are kept in a database to allow automatic or manual re-ordering before expiry.


Lookup Domain Names, DNS
------------------------

Simple DNS host look-ups using the DNS servers configured for Windows are
done using the DnsLookup method in TSslWSocket and also the ReverseDnsLookup
method, both fire an event with potentially multiple results, tested by
sample OverbyteIcsDnsLook.  These methods are used by almost all ICS
components, although they currently only use the first IPv4 or IPv6 result
returned, if more than one.

TDnsQuery in unit OverbyteIcsDnsQuery allows more complex DNS requests to
be made to specific DNS servers to get all DNS records such as MX or TXT,
tested using sample OverbyteIcsNsLookup.  It includes a list of public DNS
servers including Google, Cloudfare, OpenDNS and others, and will access
these sequentially if one does not respond.  TDnsQueryHttps in unit
OverbyteIcsSslHttpRest adds DNS over HTTPS for secure lookups. Also tested
with sample OverbyteIcsNetTools.

TIcsDomNameCache and TIcsDomNameCacheHttps cache forward and reverse DNS lookup
requests, mainly for diagnostic components but also for servers logging remote
access.  May be configured to use Windows lookup, UDP/TCP using TDnsQuery or
HTTPS, testing using samples OverbyteIcsBatchDnsLookup and OverbyteIcsNetTools.

Unit OverbyteIcsWmi contains a number of functions for accessing a Windows
DNS Server (Windows Server 2012 and later) to list DNS zones and zone
records, and to add zone records, tested by sample OverbyteIcsWmiTst.  The
functions are also used by sample OverbyteIcsX509CertsTst to add DNS records
for the ACME DNS challenge.


Network Diagnostic Tools
------------------------

The sample OverbyteIcsNetTools tests many of these diagnostic components.

TPing and TPingThread in unit OverbyteIcsPing is used to ping any host to see
if it's available on the internet, note some hosts may deliberately not reply,
tested by samples OverbyteIcsPingTst and OverbyteIcsNetTools which both include
trace route.

TIcsWhoisCli in unit OverbyteIcsWhoisCli makes Whois requests to get
details for the registrations of domain names and IP address ranges,
tested by samples OverbyteIcsWhoisCliTst and OverbyteIcsNetTools. The
component has a large list of Whois servers for various countries
around the world.

TIcsMonSocket in OverbyteIcsMonSock provides internet packet monitoring
using raw sockets.  TIcsMonPcap in OverbyteIcsMonPcap provides internet
monitoring using the Npcap NDIS driver.  There are both tested using
sample OverbyteIcsNetMon which is similar to the WireShark diagnostic tool
and can be used to monitor internet packets on a LAN, with filtering
using TIcsMonFilterClass to include or exclude IPs, port or protocols.

TIcsIpChanges in OverbyteIcsIpHlpApi monitors IP address changes and calls
an event for new IPs configured or old ones removed.  TIcsNeighbDevices
in OverbyteIcsIpHlpApi builds a historic LAN MAC device, IPv4 and IPv6
address table using ARP, neighbourhood and IP range scanning with reverse
host lookup. Both are tested with sample OverbyteIcsNetTools which also
uses several other IpHlp functions including IP Connections list, Network
Adaptors and Interfaces, IP Routing and Path tables, ARP tables and Network
Statistics.

TSnmpCli in unit OverbyteIcsSnmpCli does SNMP (simple network management
protocol), tested by sample OverbyteIcsSnmpCliTst.

TSysLogClient in unit OverbyteIcsSysLogClient send syslog packets, tested
by sample OverbyteIcsSysLogClientDemo.

TSysLogServer in unit OverbyteIcsSysLogServer receives syslog packets,
tested by sample OverbyteIcsSysLogServerDemo.

TIcsTimeClient and TIcsTimeServer in unit OverbyteIcsSntp support SNTP
for getting and setting the correct time over the internet, tested
using sample OverbyteIcsTimeTst.



Release notes
-------------

Each component and sample has his own history. You can find those histories in the comment
in the beginning of each source file. There are also a bunch of useful comments in the
source code. You should at least browse the source for the components you are interested in.

Release notes from 2017, releases V8.50 and later, can be found at the wiki site:

https://wiki.overbyte.eu/wiki/index.php/Main_Page


MidWare
-------

If you wants to build client/server applications using TCP/IP protocol, you
can do it easily with ICS. But you can do it much more easily using another
freeware product from François Piette: MidWare. Available from the same web
site https://www.overbyte.be.



