find . | grep .hs | entr sh -c 'stack build; pkill a-dark-room-exe;'
