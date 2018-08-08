
set NODE_NAME=mahjong@127.0.0.1
set COOKIE=mahjong
set CONFIG=config/sys.config

set CODE_PATH=ebin
set NUM_PROCESSES=102400
set TYPE_SMP=auto
set ETS_TABLES_MAX=100000
set ATOM_MAX=1000000

start "mahjong" werl +P %NUM_PROCESSES% -smp %TYPE_SMP% -pa %CODE_PATH% -name %NODE_NAME% -setcookie %COOKIE% -boot start_sasl -config %CONFIG% -evn ERL_MAX_ETS_TABLES %ETS_TABLES_MAX% -t %ATOM_MAX% -s main start ""
