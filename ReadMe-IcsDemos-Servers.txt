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


ICS Server Samples
------------------

IcsSslMultiWebServ           FMX Multi host HTTPS web server, uses TSslHttpAppSrv and IcsHosts components
OverbyteIcsDDWebService      Multi host HTTPS web and WebSocket server, uses TSslHttpAppSrv and IcsHosts components, maybe run as a GUI or Windows Service
OverbyteIcsIpStmLogTst       IP stream logging, sending streams as client or server using TIcsIpStrmLog and IcsHosts
OverbyteIcsProxySslServer    Proxy server for TCP and HTTP protocols using TIcsProxy and IcsHosts
OverbyteIcsSslFtpServ        FTP server with SSL/TLS, using TSslFtpServer component
OverbyteIcsSslMultiFtpServ   Multi host FTP server, using TSslFtpServer and IcsHosts components
OverbyteIcsSslMultiWebServ   Multi host HTTPS web and WebSocket server, uses TSslHttpAppSrv and IcsHosts components
OverbyteIcsSslSmtpServer     SMTP receive email server using TSslSmtpServer component
OverbyteIcsSslWebServ        HTTPS web server, uses TSslHttpServer component
OverbyteIcsBasicWebServer    Simplified version of OverbyteIcsSslMultiWebServ.


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
create self signed SSL/TLS certificates so the server can start, and will
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
includes a REST server with simple lookup responses from a SQL database, which
requires DISQLite3 to be installed.

The fifth web server is TSimpleWebSrv in unit OverbyteIcsSslHttpOAuth which
is a lightweight server with minimal functionality designed for embedding
in applications needing OAuth2 or SSL/TLS certificate ordering that require
access to web server to check a host exists.  It has a single event that
presents a request and returns a response. It supports SSL with IcsHosts.
There is no sample, but it is used by other ICS components.


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


Simple TCP Socket Server
------------------------

TIcsIpStrmLog in unit OverbyteIcsIpStreamLog is a higher level version
of TSslWSocket, originally designed for IP stream logging with minimal
events and extra coding, including an SslContext and full SSL/TLS
certificate chain checking, with better line handling, multiple
connection attempts and retries on failure or loss of connection.
TIcsIpStrmLog can be configured a client or server, TCP or UDP, and
is tested by sample OverbyteIcsIpStmLogTst which can run as client and
server at the same time, sending data to itself.


