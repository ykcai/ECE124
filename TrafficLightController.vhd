	LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

--
-- 7-segment display driver. It displays a 4-bit number on 7-segments 
-- This is created as an entity so that it can be reused many times easily
--

ENTITY SevenSegment IS PORT (
   
   dataIn      :  IN  std_logic_vector(3 DOWNTO 0);   -- The 4 bit data to be displayed
   blanking    :  IN  std_logic;                      -- This bit turns off all segments
   
   segmentsOut :  OUT std_logic_vector(6 DOWNTO 0)    -- 7-bit outputs to a 7-segment
); 
END SevenSegment;

ARCHITECTURE Behavioral OF SevenSegment IS

-- 
-- The following statements convert a 4-bit input, called dataIn to a pattern of 7 bits
-- The segment turns on when it is '0' otherwise '1'
-- The blanking input is added to turns off the all segments
--

BEGIN

   with blanking & dataIn SELECT --  gfedcba        b3210      -- D7S
      segmentsOut(6 DOWNTO 0) <=    "1000000" WHEN "00000",    -- [0]
                                    "1111001" WHEN "00001",    -- [1]
                                    "0100100" WHEN "00010",    -- [2]      +---- a ----+
                                    "0110000" WHEN "00011",    -- [3]      |           |
                                    "0011001" WHEN "00100",    -- [4]      |           |
                                    "0010010" WHEN "00101",    -- [5]      f           b
                                    "0000010" WHEN "00110",    -- [6]      |           |
                                    "1111000" WHEN "00111",    -- [7]      |           |
                                    "0000000" WHEN "01000",    -- [8]      +---- g ----+
                                    "0010000" WHEN "01001",    -- [9]      |           |
                                    "0001000" WHEN "01010",    -- [A]      |           |
                                    "0000011" WHEN "01011",    -- [b]      e           c
                                    "0100111" WHEN "01100",    -- [c]      |           |
                                    "0100001" WHEN "01101",    -- [d]      |           |
                                    "0000110" WHEN "01110",    -- [E]      +---- d ----+
                                    "0001110" WHEN "01111",    -- [F]
                                    "1111111" WHEN OTHERS;     -- [ ]

END Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY Lab5 IS
   PORT(
      
      clock_50   : IN  STD_LOGIC;
      sw         : IN  STD_LOGIC_VECTOR(17 DOWNTO 0); -- 18 dip switches on the board

      ledr       : OUT STD_LOGIC_VECTOR(17 DOWNTO 0); -- LEDs, many Red ones are available
      ledg       : OUT STD_LOGIC_VECTOR( 8 DOWNTO 0); -- LEDs, many Green ones are available
      hex0, hex2, hex4, hex6 : OUT STD_LOGIC_VECTOR( 6 DOWNTO 0)  -- seven segments to display numbers
);
END Lab5;

ARCHITECTURE SimpleCircuit OF Lab5 IS

--
-- In order to use the "SevenSegment" entity, we should declare it with first
-- 

   COMPONENT SevenSegment PORT(        -- Declare the 7 segment component to be used
      dataIn      : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      blanking    : IN  STD_LOGIC;
      segmentsOut : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
   );
   END COMPONENT;
----------------------------------------------------------------------------------------------------
CONSTANT CLK_DIV_SIZE: INTEGER := 25;     -- size of vectors for the counters

   SIGNAL Main1HzCLK:   STD_LOGIC; -- main clock to drive FSM
   SIGNAL OneHzBinCLK:  STD_LOGIC; -- binary 1 Hz clock
   SIGNAL OneHzModCLK:  STD_LOGIC; -- modulus1 Hz clock
   SIGNAL TenHzModCLK:  STD_LOGIC; -- modulus1 Hz clock
   
   SIGNAL MsetModCLK:  STD_LOGIC; -- set by SW[2-0]

   SIGNAL bin_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset binary counter to zero
   SIGNAL mod_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL onemod_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL TenMOD_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod_terminal: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset terminal count of modulus counter to zero
   
   TYPE STATES IS (STATE0, STATE1, STATE2, STATE3);   -- list all the STATES
   SIGNAL state, next_state:  STATES;                 -- current and next state signals of type STATES
   
   TYPE MODE IS (DAY,NIGHT);  -- list all the MODES where it can only be either day or night 
   SIGNAL day_state: MODE;	-- delcare the signal of type STATES
 
   TYPE DIRECTION IS (NS,EW);-- list all the DIRECTIONS where it can only be either NS or EW 
   SIGNAL def_side: DIRECTION; -- delcare the signal of type STATES
   
   SIGNAL state_number: STD_LOGIC_VECTOR(3 DOWNTO 0); -- binary state number to display on seven-segment

   SIGNAL state_counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   SIGNAL state0counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter for controller the light in state0
   SIGNAL state1counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter for controller the light in state1
   SIGNAL state2counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter for controller the light in state2
   SIGNAL state3counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter for controller the light in state3
   
   SIGNAL waitns: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   SIGNAL waitew: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment

----------------------------------------------------------------------------------------------------

BEGIN

   BinCLK: PROCESS(clock_50)
   BEGIN
      IF (rising_edge(clock_50)) THEN -- binary counter increments on rising clock edge
         bin_counter <= bin_counter + 1;
      END IF;
   END PROCESS;
   OneHzBinCLK <= std_logic(bin_counter(CLK_DIV_SIZE-1)); -- binary counter MSB, takes the most significant bit of cldivsize.
   LEDG(2) <= OneHzBinCLK;

----------------------------------------------------------------------------------------------------
                    
--   TenModCLK: PROCESS(clock_50) 
--   BEGIN
--      IF (rising_edge(clock_50)) THEN -- modulus counter increments on rising clock edge
--         IF (TENmod_counter = "0001001100010010110011111") THEN       -- half period
--            TenHzModCLK <= NOT TenHzModCLK;                 -- toggle
--            TENmod_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
--         ELSE
--            TENmod_counter <= TENmod_counter + 1;
--         END IF;
--      END IF;
--   END PROCESS;
 TenHzModCLK <= clock_50;
   OneModCLK: PROCESS(TenHzModCLK) 
   BEGIN
      IF (rising_edge(TenHzModCLK)) THEN -- modulus counter increments on rising clock edge
         IF (ONEmod_counter = 4) THEN       -- half period
            OneHzModCLK <= NOT OneHzModCLK;                 -- toggle
            ONEmod_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
         ELSE
            ONEmod_counter <= ONEmod_counter + 1;
         END IF;
      END IF;
   END PROCESS;
   LEDG(0) <= TenHzModCLK;
----------------------------------------------------------------------------------------------------
--   Main1HzCLK <= OneHzModCLK WHEN (sw(1) = '0') ELSE OneHzBinCLK; -- assigns either binary or modulus clock as main clock
--   LEDG(1) <= Main1HzCLK;
--------------------------------------------------------------------------------------------------
   DAYANDNIGHT: PROCESS(state, sw) -- main FSM
   BEGIN
      next_state <= state;
      --ledr(15 DOWNTO 0) <= "0000000000000000";
      
      IF (sw(17) = '0') THEN
		 day_state <= DAY;
	  ELSE
		 day_state <= NIGHT;
      END IF;
      
      IF (sw(16) = '0') THEN
		 def_side <= NS;
	  ELSE
		 def_side <= EW;
      END IF;
      
	
   CASE state IS
	
	  WHEN STATE0=> -- first 8 seconds of 16 second cycle for day time
	  -- this is also the first 8 seconds of transition into NS default night mode
		 state_number <= "0000";
		 IF (state0counter < "0010" ) THEN -- 2 second green NS flashing
			ledg(8) <= TenHzModCLK;
			ledr(11) <= '0';
			ledg(7) <= '0';
			ledr(0) <= '1';
			next_state <= STATE0;
         ELSIF (state0counter > "0001" and state0counter < "0110" ) THEN -- 4 second green NS solid
			ledg(8) <= '1';
			ledr(11) <= '0';
			ledg(7) <= '0';
			ledr(0) <= '1';
			next_state <= STATE0;
		 ELSIF (state0counter > "0101" and state0counter < "0111" ) THEN -- 2 second red NS flashing
			ledg(8) <= '0';
			ledr(11) <= TenHzModCLK;
			ledg(7) <= '0';
			ledr(0) <= '1';
			next_state <= STATE0;
			-- the code above will get repeated when we transition to state 2 where night mode has NS defaulted
			-- thus this will give the 2greenflash-4greensolid-2redflash sequence as state in the manual
		 ELSE  -- 2 second red NS flashing still but now this has conditions as when the switch 17 is turned on ]
		 -- this is the 7th second
			ledg(8) <= '0';
			ledr(11) <= TenHzModCLK;
			ledg(7) <= '0';
			ledr(0) <= '1';		
			IF (day_state = NIGHT and def_side = NS and sw(14) = '0' ) THEN -- if night mode is activated 
														--and the default side in NS and there are no cars waiting on the EW side
														--in this statement we establish the transition to state 2
				next_state <= STATE2;
			ELSE
				next_state <= STATE1;
			END IF;
		 END IF;
		 
	  WHEN STATE1=> -- last 8 seconds of 16 second cycle for day time
	  -- this is also the first 8 seconds of transition into EW default night mode
		 state_number <= "0001";
		 IF (state1counter < "0010") THEN -- 2 second green EW flashing 
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= TenHzModCLK;
			ledr(0) <= '0';
			next_state <= STATE1;
         ELSIF (state1counter > "0001" and state1counter < "0110") THEN -- 4 seconds of green EW solid
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '1';
			ledr(0) <= '0';
			next_state <= STATE1;
		 ELSIF (state1counter > "0101" and state1counter < "0111") THEN-- 2 seconds of red EW flashing
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '0';
			ledr(0) <= TenHzModCLK;
			next_state <= STATE1;
			-- the code above will get repeated when we transition to state 3 where night mode has NS defaulted
			-- thus this will give the 2greenflash-4greensolid-2redflash sequence as state in the manual
		 ELSE -- -- 2 seconds of red EW flashing but now what happens after this changes based on the conditions of next state
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '0';
			ledr(0) <= TenHzModCLK;	
			IF (day_state = NIGHT and def_side = EW and sw(15) = '0') THEN -- if day is changed to night,
										--and its defaulted on the EW side while no cars are waiting on the NS side
										--in this statement we establish the transition to state 3
				next_state <= STATE3;
			ELSE
				next_state <= STATE0;
			END IF;
		 END IF;		 
		 
	  WHEN STATE2=>
	     state_number <= "0010"; --night mode at the NS when NS is default 
         IF (state2counter < "0110") THEN -- 6 seconds of green NS solid at night mode
			ledg(8) <= '1';
			ledr(11) <= '0';
			ledg(7) <= '0';
			ledr(0) <= '1';
			next_state <= STATE2;
		 ELSIF (state2counter > "0101" and state2counter < "0111") THEN -- 2 seconds of green NS flashing at night mode
			ledg(8) <= '0';
			ledr(11) <= TenHzModCLK;
			ledg(7) <= '0';
			ledr(0) <= '1';
			next_state <= STATE2;
		 ELSE -- then during the transition we have to meet the conditions of if the default changes, mode changes or if there is a car waiting on the EW side 
			ledg(8) <= '0';
			ledr(11) <= TenHzModCLK;
			ledg(7) <= '0';
			ledr(0) <= '1';		
			IF (day_state = NIGHT and def_side = NS and sw(14) = '0') THEN
				next_state <= STATE2; -- if all the condition reemain the same then we dont change states, so it will stay in state 2
			ELSE
				next_state <= STATE1; -- else when something changes such as day - night or there is a car waiting or default side changes, we change to state 1
				--when we are in state 2 and day_state is DAY we go to state 1 because we have to turn the EW lights to green as they were red before
				--we change to state 1 because from state 1 we can change to state 3 for the EW defaulted side
			END IF;
		 END IF;
		 
	  WHEN STATE3=> --night mode when EW is default 
		 state_number <= "0011"; 
         IF (state3counter < "0110") THEN  -- 6 seconds of green EW solid at night mode
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '1';
			ledr(0) <= '0';
			next_state <= STATE3;
		 ELSIF (state3counter > "0101" and state3counter < "0111") THEN -- 2 seconds of green EW flashing at night mode
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '0';
			ledr(0) <= TenHzModCLK;
			next_state <= STATE3;
		 ELSE -- then during the transition we have to meet the conditions of if the default changes, mode changes or if there is a car waiting on the EW side 
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '0';
			ledr(0) <= TenHzModCLK;
			IF (day_state = NIGHT and def_side = EW and sw(15) = '0') THEN -- if all the condition reemain the same then we dont change states, so it will stay in state 3
				next_state <= STATE3;
			ELSE
				next_state <= STATE0; -- else when something changes such as day - night or there is a car waiting or default side changes, we change to state 0 where we can 
				--then transition to state 2
				--when we are in state 3 and day_state is DAY we go to state 0 because we have to turn the NS lights to green as they were red before
				--we change to state 0 because from state 0 we can change to state 2 for the NS defaulted side
			END IF;
		 END IF;
		 		  
	END CASE;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   SeqLogic: PROCESS(OneHzModCLK, state) -- creats sequential logic to latch the state
   BEGIN
      IF (rising_edge(OneHzModCLK)) THEN       
         
         state <= next_state;                      -- on the rising edge of clock the current state is updated with next state
         IF( state /= next_state)THEN -- the current state would not equal the next state, so it would reset to 0 when it reaches the next state
         state_counter <= "0000";
        
		  --state_counter <= state_counter + 1;   -- on the rising edge of clock the current counter is incremented
		 
		 ELSIF (state = STATE0) THEN -- this is the counter for the light control in state0
			IF (state0counter = "1000") THEN -- reset the counter when it reaches 
			state0counter <= "0000";
			ELSE
				state0counter <= state0counter + 1;	
			END IF;
			state_counter <= state_counter + 1;
			state1counter <= "0000"; -- set all other counters for other states to 0
			state2counter <= "0000";-- set all other counters for other states to 0
			state3counter <= "0000";-- set all other counters for other states to 0
			
		 ELSIF (state = STATE1) THEN-- this is the counter for the light control in state1
			IF (state1counter = "1000") THEN
				state1counter <= "0000";
			ELSE
				state1counter <= state1counter + 1;		
			END IF;
			state_counter <= state_counter + 1;
			state0counter <= "0000";-- set all other counters for other states to 0
			state2counter <= "0000";-- set all other counters for other states to 0
			state3counter <= "0000";-- set all other counters for other states to 0
			
		 ELSIF (state = STATE2) THEN-- this is the counter for the light control in state2
			IF (state2counter = "0111") THEN
				state2counter <= "0000";
			ELSE
				state2counter <= state2counter + 1;		
			END IF;
			state_counter <= state_counter + 1;
			state0counter <= "0000";-- set all other counters for other states to 0
			state1counter <= "0000";-- set all other counters for other states to 0
			state3counter <= "0000";-- set all other counters for other states to 0

		 ELSIF (state = STATE3) THEN-- this is the counter for the light control in state3
			IF (state3counter = "0111") THEN
				state3counter <= "0000";
			ELSE
				state3counter <= state3counter + 1;		
			END IF;
			state_counter <= state_counter + 1;
			state0counter <= "0000";-- set all other counters for other states to 0
			state1counter <= "0000";-- set all other counters for other states to 0
			state2counter <= "0000";-- set all other counters for other states to 0
		 END IF;
		 
		IF ((state = STATE2 and sw(14) = '1') or  (state = STATE0 and sw(14) = '1')) THEN
			-- the wait counter will always increment when there is a car waiting on the EW side when there is the red light on EW and green light on NS
			-- this also accounts for the day mode traffic light, as well as night mode traffic light after the transition from having a car waiting on the non-default side
			waitns <= "0000";
			waitew <= waitew + 1;
			IF( state /= next_state) THEN
				waitew <= "0000";
			END IF;
		ELSIF ((state = STATE3 and sw(15) = '1') or (state = STATE1 and sw(15) = '1')) THEN
			-- the wait counter will always increment when there is a car waiting on the NS side when there is the red light on NS and green light on EW
			-- this also accounts for the day mode traffic light, as well as night mode traffic light after the transition from having a car waiting on the non-default side
			waitew <= "0000";
			waitns <= waitns + 1;	
			IF( state /= next_state) THEN
				waitns <= "0000";
			END IF;
		ELSE
			waitns <= "0000"; -- reset counters otherwise
			waitew <= "0000";	
		END IF;
      
      END IF;

   END PROCESS;

----------------------------------------------------------------------------------------------------
LEDR(4 downto 1)  <= state_number;
LEDR(8 downto 5)  <= std_logic_vector(waitew);
LEDR(15 downto 12)<= std_logic_vector(waitns);
LEDG(6 downto 3)  <= std_logic_vector(state_counter);
   statenumber: SevenSegment PORT MAP( state_number, '0', hex0 );
   waitcounterns: SevenSegment PORT MAP( std_logic_vector(waitew), '0', hex6 );
   waitcounterew: SevenSegment PORT MAP( std_logic_vector(waitns), '0', hex4 );
   statecounter: SevenSegment PORT MAP( std_logic_vector(state_counter), '0', hex2 );



END SimpleCircuit;
