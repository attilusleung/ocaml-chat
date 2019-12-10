## Installation Instructions

### Installing OPAM packages
Run the command `opam install oUnit ounit-lwt ANSITerminal lwt lwt_ppx` in the
command line. The following OPAM packages will be installed:
- oUnit
- ounit-lwt
- ANSITerminal
- lwt
- lwt_ppx

### Running the chatroom
NOTE: There is currently a known issue with running the client on WSL (because
WSL doesn't handle unix sockets properly). While the code will work, attempting
to run the code without an active server will cause the program to crash.
Therefore, the server should be run before the client on WSL systems. This is
not an issue on linux, and the client will continuously try and connect to the
server until it can connect successfully. Honestly, I should probably dockerize
this.

#### Running the server
Before running the server, please make sure the port 9000 on the system is not
currently in use.
Run `make serve`.

#### Running the client
Run `terminal.native --help` to print the help and usage of the client after
building the client with `make term`.

Run `make term` to create a chat client that connects to any server run on the
local computer.

Alternatively, a server is already hosted online. To connect to this server, run
`make remote`.

#### Usage
Navigating the TUI of the client generally uses vim key bindings (`h`, `j`, `k`,
`l` to move, `o` to open/interact.). Alternatively, arrow keys and `Enter` can
be used as well. 

The only part of the ui that may not be as self explanatory is the status bar;
after logging in, no user is selected by default, which means your messages are
sent to a buffer that does not go to anybody. Focus on the status bar using
`Ctrl-S`, and select any of the online users that show up there. This would
allow you to view, as well as send messages to that user. 
