# Trippples
 A recreation of the game Trippples in Racket.
 Two client worlds may connect to a server at once.
 On startup of the client launc, a login window will show.
 This window closes when you attempt to log into the server.
 Additional clients are immediately booted off. No spectator mode here.
 Clients send a packet of two integers to the server.
 First represents the cursor location. The second the selected tile.
 The server sends the new world state after each valid update.
 This includes the board position, player token positions, unplaced tiles, and turn order.