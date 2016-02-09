library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--
-- 7-segment display driver. It displays a 4-bit number on 7-segments 
-- This is created as an entity so that it can be reused many times easily
--

entity SevenSegment is port (
   
   dataIn      :  in  std_logic_vector(3 downto 0);   -- The 4 bit data to be displayed
   blanking    :  in  std_logic;                      -- This bit turns off all segments
   
   segmentsOut :  out std_logic_vector(6 downto 0)    -- 7-bit outputs to a 7-segment
); 
end SevenSegment;

architecture Behavioral of SevenSegment is

-- 
-- The following statements convert a 4-bit input, called dataIn to a pattern of 7 bits
-- The segment turns on when it is '0' otherwise '1'
-- The blanking input is added to turns off the all segments
--

begin

   with blanking & dataIn select --  gfedcba        b3210      -- D7S
      segmentsOut(6 downto 0) <=    "1000000" when "00000",    -- [0]
                                    "1111001" when "00001",    -- [1]
                                    "0100100" when "00010",    -- [2]      +---- a ----+
                                    "0110000" when "00011",    -- [3]      |           |
                                    "0011001" when "00100",    -- [4]      |           |
                                    "0010010" when "00101",    -- [5]      f           b
                                    "0000010" when "00110",    -- [6]      |           |
                                    "1111000" when "00111",    -- [7]      |           |
                                    "0000000" when "01000",    -- [8]      +---- g ----+
                                    "0010000" when "01001",    -- [9]      |           |
                                    "0001000" when "01010",    -- [A]      |           |
                                    "0000011" when "01011",    -- [b]      e           c
                                    "1000110" when "01100",    -- [c]      |           |
                                    "0100001" when "01101",    -- [d]      |           |
                                    "0000110" when "01110",    -- [E]      +---- d ----+
                                    "0001110" when "01111",    -- [F]
                                    "1111111" when others;     -- [ ]

end Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Lab2 is port (
      
      sw              				 :   in  std_logic_vector(17 downto 0); -- 18 dip switches

      ledr             				 :   out std_logic_vector(17 downto 0); -- 18 red LEDs
      hex0, hex1, hex2, hex4, hex5, hex6, hex7	     :   out std_logic_vector( 6 downto 0)  -- 7-segment displays
);
end Lab2;

architecture SimpleCircuit of Lab2 is

--
-- In order to use the "SevenSegment" entity, we should declare it with first
-- 

   component SevenSegment port (
      dataIn      :  in    std_logic_vector(3 downto 0); --input signals
      blanking    :  in    std_logic; -- input signals
      segmentsOut :  out   std_logic_vector(6 downto 0) -- output signals
   );
   end component;
--
   signal CarryOut : std_logic_vector(3 downto 0); -- signal declaration of a 8 bit signal
   signal Operand1, Operand2 : std_logic_vector(7 downto 0); -- signal declaration of a 8 bit signal
   signal OperationResult :std_logic_vector(11 downto 0); -- signal declaration of a 12 bit signal
   signal Operator: std_logic_vector(1 downto 0); -- signal declaration of a 2 bit signal


begin


   Operand1 <= sw(7 downto 0);  -- from switches 7 to 0, each will represent a bit in the signal lowest being 0
   Operand2 <= sw(15 downto 8);  -- from switches 15 to 8, each will represent a bit in the 12 bit signal
   Operator <= sw(17 downto 16); -- 2 bit input with switches 17 and 16
   CarryOut <= OperationResult(11 downto 8);
   ledr(17 downto 16) <= Operator; -- the red led will turn on when the switches 17 and/or 16 are ON. 
   -- S is only two signals (two switches)
   
	with sw(17 downto 16) select                       
		OperationResult(11 downto 0) <=  	"0000"&Operand1 and "0000"&Operand2 									when	"00", -- when the operator switches are in "00" the two 
																								  --signal are being AND together
											"0000"&Operand1 or  "0000"&Operand2 									when 	"01", -- when the operator switches are in "01" the two 
																								  --signal are being OR together
											"0000"&Operand1 xor "0000"&Operand2 									when	"10",-- when the operator switches are in "10" the two 
																								  --signal are being XOR together
						   std_logic_vector(unsigned("0000"&Operand1)+unsigned("0000"&Operand2))  when    "11";-- when the operator switches are in "11" the two 
																								  --signal are being ADDED together
						   
   ledr(11 downto 0) <= OperationResult(11 downto 0);				    
   D7SH0: SevenSegment port map(OperationResult(3 downto 0), '0', hex0 ); -- Result is diplayed on HEX0, blanking is disabled
   D7SH1: SevenSegment port map(OperationResult(7 downto 4), '0', hex1 ); -- Result is diplayed on HEX0, blanking is disabled
   D7SH2: SevenSegment port map(OperationResult(11 downto 8), not CarryOut(0), hex2 ); 
   D7SH3: SevenSegment port map(Operand1(3 downto 0), '0', hex4 ); -- A is diplayed on HEX4, blanking is disabled
   D7SH4: SevenSegment port map(Operand1(7 downto 4), '0', hex5 ); -- A is diplayed on HEX5, blanking is disabled
   D7SH5: SevenSegment port map(Operand2(3 downto 0), '0', hex6 ); -- B is diplayed on HEX6, blanking is disabled
   D7SH6: SevenSegment port map(Operand2(7 downto 4), '0', hex7 ); -- B is diplayed on HEX7, blanking is disabled


end SimpleCircuit;
