http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles




$ cd ../ebin
$ erl -boot start_sasl

 1> appmon:start().
 {ok,<0.44.0>}
 2> application:start(obelisk).
 ok
 
 3> {ok,S} = gen_tcp:connect({127,0,0,1},2223,[{packet,2}]).
 {ok,#Port<0.150>}
 
 4> gen_tcp:send(S,<<"hello">>).
 ok
 5> f(M), receive M -> M end.
 {tcp,#Port<0.150>,"hello"}
 
 
 --------------------------------------------------------------------------
 
 Opts_true = [binary, {packet, 2},
                        {reuseaddr, true}, {keepalive, true},
                        {backlog, 30}, {active, false}, {use_ssl, true},
                        {depth, 2},
                        {certfile,   "../client-cert.pem"}, 
                        {keyfile,    "../client-key.pem"},
                        {cacertfile, "../cacert.pem"}].
 
 Opts_true = [binary, {packet, 2},
                        {reuseaddr, true}, {keepalive, true},
                        {backlog, 30}, {active, false}, {use_ssl, true}].
 Opts_true = [binary, {packet, 2}, {active, false}, {use_ssl, true}].
 
 mijktcp:connect({127,0,0,1}, 2225, Opts_true).
 
 
 
 
 
 -------------------------------------------------------------------------------
 
 obelisk should get path to config file from env or command line
 
 -------------------------------------------------------------------------------
 GIT
Global setup:
  Download and install Git
  git config --global user.name "Your Name"
  git config --global user.email mijkenator@gmail.com
  Add your public key
        
Next steps:
  mkdir Obeerl
  cd Obeerl
  git init
  touch README
  git add README
  git commit -m 'first commit'
  git remote add origin git@github.com:mijkenator/Obeerl.git
  git push origin master
      
Existing Git Repo?
  cd existing_git_repo
  git remote add origin git@github.com:mijkenator/Obeerl.git
  git push origin master
      
Importing a SVN Repo?
  Click here
      
When you're done:
  Continue
  
 http://github.com/mijkenator/Obeerl/tree/master
--------------------------------------------------------------------------------




git status
git add
git commit -a -m "comment"
git push origin master

git rm ebin/\*.beam

git pull