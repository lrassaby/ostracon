Ostracon
========

This submission consists of two examples, each of which has its own copy of all 
Ostracon files, as well as client side code. For a more detailed expanation of
the how Ostracon's modules interact, see project-final-report.pdf.

Team name: Ostracon (a.k.a. FunkMasterPrimeDeluxe)
- Louis Rassaby
- Scott Jacobson
- Daniel Kim
- Jeremy Max Goldman

Fork us on Github! https://github.com/lrassaby/ostracon

###Dependencies (installed automatically during the build process):
- Cowboy for HTTP connections
    - Cowlib
    - Ranch
- Jiffy for JSON creation
Built using the relx package manager.

###Build instructions
1. cd to the root directory of the submission
2. type "Make intro" to build and run the intro demo on localhost:8081
3. type "Make professors" to build and run "Let's Have Some Funding" on 
   localhost:8080
During the build process, relx will grab dependencies from Github. 
If you're having trouble with dependencies, cd to introdemo or professors and 
remove the deps folder. Then try the the build process again.

###File structure for significant components
There are other components, but most are for packaging the app for distribution
with relx.
 |- Makefile (directions above)
 |- Design (contains previous designs)
 |- introdemo (contains the intro demo with graphs and presentation)
    |- priv (contains static assets for the front end)
       |- index.html 
       |- favicon.ico (not really important, but 90s style favicon)
       |- assets
          |- js
             |- index.js (contains application-specific code)
             |- ostracon.js (contains Ostracon's front end library)
    |- src (contains the ostracon library, plus the callback module)
       |- callback_module.erl
       |- ostracon.app.src
       |- ostracon_app.erl
       |- ostracon_collector.erl
       |- ostracon_handler.erl
       |- ostracon_sup.erl
 |- professors (contains the multiplayer game)
    |- priv (contains static assets for the front end)
       |- index.html 
       |- favicon.ico (not really important, but 90s style favicon)
       |- assets
          |- js
             |- index.js (contains application-specific code)
             |- ostracon.js (contains Ostracon's front end library)
    |- src (contains the ostracon library, plus the callback module)
       |- callback_module.erl
       |- ostracon.app.src
       |- ostracon_app.erl
       |- ostracon_collector.erl
       |- ostracon_handler.erl
       |- ostracon_sup.erl
###File descriptions

Ostracon.js
    This is the front end JavaScript module that connects with 
    application-specific front end code. It sends votes to the handler and 
    receives the updated state from the handler to give to the client. The 
    Ostracon front end library exports the following functions:

        ostracon.start();
        ostracon.getState();
        ostracon.pushVote(vote);
        ostracon.requestState();

ostracon_app.erl
    This module starts off the voting procedure by creating the Vote ETS and 
    State ETS, then initiates the collector module. It also establishes a link 
    with a supervisor. Finally, it sets up Cowboy to spawn instances of Ostracon Handler upon connection requests.

ostracon_sup.erl
    This module monitors Ostracon App for failure and has a one-for-one restart 
    policy with it.

ostracon_collector.erl
    The collector gathers votes from the Vote ETS every cycle and computes a vote histogram to send to the callback module. The length of each cycle is determined by the Callback Module.

ostracon_handler.erl
    This module communicates with the client via WebSocket; it receives the client's vote to record in the Vote ETS and sends the updated state to the client. Ostracon Handler also sends the current state to any new client that joins in the middle. 

callback_module.erl
    This is the back end written by the application developer using Erlang. It determines how each vote should affect state, writing state updates directly 
    to the State ETS.

Front end code
    Application-specific front end logic that communicates with the back end using 
    Ostracon.js.