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

ENTITY Lab4 IS
   PORT(
      
      clock_50   : IN  STD_LOGIC;
      sw         : IN  STD_LOGIC_VECTOR(17 DOWNTO 0); -- 18 dip switches on the board

      ledr       : OUT STD_LOGIC_VECTOR(17 DOWNTO 0); -- LEDs, many Red ones are available
      ledg       : OUT STD_LOGIC_VECTOR( 8 DOWNTO 0); -- LEDs, many Green ones are available
      hex0, hex2 : OUT STD_LOGIC_VECTOR( 6 DOWNTO 0)  -- seven segments to display numbers
);
END Lab4;

ARCHITECTURE SimpleCircuit OF Lab4 IS

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

   SIGNAL Main1HzCLK:   STD_LOGIC; -- main 1Hz clock to drive FSM
   SIGNAL OneHzBinCLK:  STD_LOGIC; -- binary 1 Hz clock
   SIGNAL OneHzModCLK:  STD_LOGIC; -- modulus1 Hz clock
   SIGNAL TenHzModCLK:  STD_LOGIC; -- modulus10 Hz clock

   SIGNAL bin_counter:  	UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) 	 := to_unsigned(0,CLK_DIV_SIZE); -- reset binary counter to zero
   SIGNAL TENmod_counter:   UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) 	 := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus10 counter to zero
   SIGNAL ONEmod_counter:   UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) 	 := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus1 counter to zero
  -- SIGNAL mod_terminal: 	UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) 	 := to_unsigned(0,CLK_DIV_SIZE); -- reset terminal count of modulus counter to zero
   
   TYPE STATES IS (STATE0, STATE1, STATE2, STATE3, STATE4, STATE5);   -- list all the STATES
   SIGNAL state, next_state:  STATES;                 -- current and next state signals od type STATES
   
   SIGNAL state_number: STD_LOGIC_VECTOR(3 DOWNTO 0); -- binary state number to display on seven-segment

   SIGNAL state_counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   SIGNAL timer: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   
----------------------------------------------------------------------------------------------------

BEGIN

   BinCLK: PROCESS(clock_50)
   BEGIN
      IF (rising_edge(clock_50)) THEN -- binary counter increments on rising clock edge
         bin_counter <= bin_counter + 1;
      END IF;
   END PROCESS;
   OneHzBinCLK <= std_logic(bin_counter(CLK_DIV_SIZE-1)); -- binary counter MSB
   LEDG(2) <= OneHzBinCLK;

----------------------------------------------------------------------------------------------------
                    
   TenModCLK: PROCESS(clock_50) 
   BEGIN
      IF (rising_edge(clock_50)) THEN -- modulus counter increments on rising clock edge
         IF (TENmod_counter = "0001001100010010110011111") THEN       -- half period
            TenHzModCLK <= NOT TenHzModCLK;                 -- toggle
            TENmod_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
         ELSE
            TENmod_counter <= TENmod_counter + 1;
         END IF;
      END IF;
   END PROCESS;
   --TenHzModCLK <= clock_50;
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
   Main1HzCLK <= OneHzModCLK WHEN (sw(17) = '0') ELSE OneHzBinCLK; -- assigns either binary or modulus clock as main clock
   LEDG(1) <= Main1HzCLK;
----------------------------------------------------------------------------------------------------
   FSM: PROCESS(Main1HzCLK, state) -- main FSM
   BEGIN
   ledr(15 DOWNTO 0) <= "0000000000000000";
      CASE state IS
         WHEN STATE0 =>
            state_number <= "0000";
            LEDG(8) <= TenHzModCLK;
            LEDG(7) <= '0';
            LEDR(11) <= '0';
            LEDR(0) <= '1'; 

         WHEN STATE1 =>
            state_number <= "0001";
            LEDG(8) <= '1';
            LEDR(11) <= '0';
            LEDG(7) <= '0';
            LEDR(0) <= '1';
		 
         WHEN STATE2 =>
            state_number <= "0010";
            LEDG(8) <= '0';
            LEDR(0) <= '1';
            LEDG(7) <= '0';
            LEDR(11)<= TenHzModCLK;
		 
         WHEN STATE3 => 
            state_number <= "0011";
            LEDR(11) <= '1';
            LEDG(8) <= '0';
            LEDR(0) <= '0';
            LEDG(7) <= TenHzModCLK; 
		  
         WHEN STATE4 => 
            state_number <= "0100";
            LEDR(11) <= '1';
            LEDG(8) <= '0';
            LEDG(7) <= '1';
            LEDR(0) <= '0';
		  
         WHEN STATE5 => 
            state_number <= "0101";
            LEDR(11)<= '1';
            LEDG(8) <= '0';
            LEDG(7) <= '0';
            LEDR(0) <= TenHzModCLK;
		 
      END CASE;
        END PROCESS;
----------------------------------------------------------------------------------------------------
   SeqLogic: PROCESS(Main1HzCLK, state) -- creats sequential logic to latch the state
   BEGIN
      IF (rising_edge(Main1HzCLK)) THEN
            state_counter <= state_counter + 1;    -- on the rising edge of clock the current counter is incremented
         IF(state_counter = "0001")THEN
         state <= STATE0;
         ELSIF(state_counter = "0010")THEN
         state <= STATE1;
         ELSIF(state_counter = "0110")THEN
         state <= STATE2;
         ELSIF(state_counter = "1000")THEN
         state <= STATE3;
         ELSIF(state_counter = "1010")THEN
         state <= STATE4;
         ELSIF(state_counter = "1110")THEN
         state <= STATE5;
         ELSIF(state_counter = "0000")THEN
         state <= STATE0;
         state_counter <= "0001";
         END IF;
      END IF;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   D7S0: SevenSegment PORT MAP( state_number, '0', hex0 );
   D7S4: SevenSegment PORT MAP( std_logic_vector(state_counter), '0', hex2 );

END SimpleCircuit;
