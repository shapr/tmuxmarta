tmuxmarta
=========
tmuxmarta displays upcoming MARTA trains in your byobu status line.

- [x] show basic information
- [x] read api key from the filesystem
- [ ] read MARTA station config from the filesystem
- [ ] tmux prettify output

You can get an API key from http://www.itsmarta.com/app-developer-resources.aspx .

For the sake of completing this quick hack, your MARTA api key is read from the file `$HOME/.martakey` and your choice of MARTA stops are configured by changing the `config` variable in Lib.hs. The stops I've seen so far are listed below, and my config value looks like `config = [("MIDTOWN STATION",N), ("NORTH AVE STATION",S), ("LINDBERGH STATION",S)]`.

Once you run `stack install` you will be able to copy the binary `tmuxmarta-exe` into your `~/.byobu/bin/` directory as something sensible like `300_marta` at which point, every 300 seconds your byobu status line will display basic information about your MARTA stops.

- "AIRPORT STATION"
- "ARTS CENTER STATION"
- "ASHBY STATION"
- "AVONDALE STATION"
- "BANKHEAD STATION"
- "BROOKHAVEN STATION"
- "BUCKHEAD STATION"
- "CHAMBLEE STATION"
- "CIVIC CENTER STATION"
- "COLLEGE PARK STATION"
- "DECATUR STATION"
- "DORAVILLE STATION"
- "DUNWOODY STATION"
- "EAST LAKE STATION"
- "EAST POINT STATION"
- "EDGEWOOD CANDLER PARK STATION"
- "FIVE POINTS STATION"
- "GARNETT STATION"
- "GEORGIA STATE STATION"
- "HAMILTON E HOLMES STATION"
- "INDIAN CREEK STATION"
- "INMAN PARK STATION"
- "KENSINGTON STATION"
- "KING MEMORIAL STATION"
- "LAKEWOOD STATION"
- "LENOX STATION"
- "LINDBERGH STATION"
- "MEDICAL CENTER STATION"
- "MIDTOWN STATION"
- "NORTH AVE STATION"
- "NORTH SPRINGS STATION"
- "OAKLAND CITY STATION"
- "OMNI DOME STATION"
- "PEACHTREE CENTER STATION"
- "SANDY SPRINGS STATION"
- "VINE CITY STATION"
- "WEST END STATION"
- "WEST LAKE STATION"
