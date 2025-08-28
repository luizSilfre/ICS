ICS - Internet Component Suite - V9.3 - Delphi 7 to RAD Studio 12
=================================================================
(Aka FPIETTE's Components)

Revised: September 25, 2024
Release: V9.3
https://www.overbyte.eu/
https://wiki.overbyte.eu/
https://svn.overbyte.be/svn/


ICS is a free internet component library for all Delphi, C++Builder, BDS and RAD Studio versions.
It includes TCP, UDP, raw sockets, clients, servers, as well as all the main high level protocols
such as FTP, SMTP, POP3, NNTP, HTTP and more. ICS also supports SSL/TLS with the help of OpenSSL.

ICS contains many sample applications developed over 25 years, used to develop and test ICS for
Windows VCL and FMX applications.


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


ICS Client Samples
------------------

FrameBrowserIcs              Web Browser using TSslHttpCli and HtmlViewer component
IcsHttpRestTstFmx            FMX HTTPS web, REST and OAuth, Send SMS, Rest Email, and DoH demos
IcsHttpsTst                  FMX HTTP web GET using THttpCli component, tests all commands
OverbyteIcsHttpRestTst       HTTPS web, REST and OAuth, WebSocket, Send SMS, Rest Email, and DoH demos.
OverbyteIcsHttpThrd          Threaded HTTPS using TSslHttpRest.
OverbyteIcsHttpsTst          HTTPS using TSslHttpCli component, tests all commands
OverbyteIcsIpStmLogTst       IP stream logging, sending streams as client or server using TIcsIpStrmLog and IcsHosts
OverbyteIcsMailQuTst         Mailing list tool using TIcsMailQueue component with SMTP to send email
OverbyteIcsMQTTst            MQ Telemetry Transport message queuing service
OverbyteIcsSnippets          Small samples of codes for FTP, HTTP, sockets and email
OverbyteIcsSslFtpTst         FTP client with SSL/TLS, using TSslFtpClient component, tests all commands
OverbyteIcsSslMailRcv        POP3 receive email client using TSslPop3Cli component, tests all commands
OverbyteIcsSslMailSnd        SMTP send email client using TSslSmtpCli component, tests all commands
OverbyteIcsSslNewsRdr        NNTP Network News Transport client, using TSslNntpCli, tests all commands
OverbyteIcsXferTst           File copying, FTP upload and download, HTTP download, using TIcsFileCopy, TIcsFtpMulti and TIcsHttpMulti
OverbyteIcsConHttp           Shows how to use TSslHttpRest component within a console mode application.


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


World Wide Web, HTTP Client
---------------------------

There are four types of HTTP component, with many extra components used to
extend their capabilities.

TSslHttpCli in unit OverbyteIcsHttpProt is the low level HTTP protocol client
that is tested using sample OverbyteIcsHttpsTst. It has buttons for GET and HEAD
commands and allows numerous SSL parameters to be specified. POST requests are
tested with samples OverbyteIcsHttpPost and OverbyteIcsHttpPg. Other units
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
checkers as you type, search hints, etc.

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


Simple TCP Socket Client
------------------------

TIcsIpStrmLog in unit OverbyteIcsIpStreamLog is a higher level version
of TSslWSocket, originally designed for IP stream logging with minimal
events and extra coding, including an SslContext and full SSL/TLS
certificate chain checking, with better line handling, multiple
connection attempts and retries on failure or loss of connection.
TIcsIpStrmLog can be configured a client or server, TCP or UDP, and
is tested by sample OverbyteIcsIpStmLogTst which can run as client and
server at the same time, sending data to itself.


MQ Telemetry Transport
----------------------

TIcsMQTTServer and TIcsMQTTClient in OverbyteIcsMQTT handle the MQ Telemetry
Transport message queuing service, tested by sample OverbyteIcsMQTTst which
has both client and server,



Network News Reader, NNTP Client
--------------------------------

TSslNntpCli in unit OverbyteIcsNntpCli is a NNTP client, tested by
sample OverbyteIcsSslNewsRdr with 28 buttons for the various commands,


