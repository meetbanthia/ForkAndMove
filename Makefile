main: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 main.mlb

test: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 test.mlb

tictactoe : *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 tictactoe.mlb