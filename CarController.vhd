library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;

-- The above libaries lines must be included in every VHDL file, before EVERY ENTITY!

--
-- This is the main circuit Entity, which connects all wires to the FPGA pins (lights and switches)
-- First we have a PORT mapping - naming all wires going to the outside world and if they are INputs or OUTputs
-- Note that all signal names here are fixed by the "DE2_pins.csv" file which you must use for every lab
--   

entity Lab1 is port(
      
      key   : in  std_logic_vector(2 downto 0); -- 3 push buttons on the board - normally HIGH or '1' when not pressed
      sw    : in  std_logic_vector(0 downto 0); -- use 1 out of 18 switches on the board - '0' (LOW) when down towards edge of board

      ledr  : out std_logic_vector(0 downto 0); -- Red LEDs, only 2 used
      ledg  : out std_logic_vector(0 downto 0)  -- Green LEDs, only 2 used
);
end Lab1;

architecture CarControls of Lab1 is

--

signal gas, clutch, brake, overrideSwitch: std_logic; -- four signals for inputs
signal GasControl, BrakeControl: std_logic;  -- two signals for LED outputs

-- The function of Lab1 entity is defined here
	
begin

   
   gas 		<=  key(0);    --gas
   clutch 	<=  key(1);    --clutch
   brake 	<=  key(2);    --brake
   overrideSwitch <= sw(0); --override switch

	-- 0 for led is OFF, 1 for led is ON
	-- 0 for switch is OFF, 1 for switch is ON
	-- 0 for key is ON, 1 for key is OFF

	GasControl <= (not gas) and clutch and  brake and (not overrideSwitch);
	--gas is only ON when one the gas key is pressed 
	--either brake or override is ON, gas should not light up
	BrakeControl <= overrideSwitch or (not brake);
	
	-- 2 logic elements 

   ledr(0) <= BrakeControl;    -- red_led0 is assigned to output port (pin) ledr(0)
   ledg(0) <= GasControl;    -- green_led0 is assigned to output port (pin) ledg(0)

end CarControls;
