/*
  Blinken.cpp - Das Blinkenlichten RGB LED support library
  Copyright (c) 2009 T. R. Gipson.  All right reserved.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3.0 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/*


CHANGES

2/3/2009:
Cleaned up some comments, added "reset" method, packaged up example sketch to go with the library.

1/15/2009:
Got an actual Arduino to test with.
Reimplemented as library (classname Blinken, old prefixes removed)
Fixed up Identify

8/24/2008:
Added defines for commands, functions for writing direct RGB value and commands.
Prefixed all functions with 'blinken_' to avoid possible future name collisions.

TODO:
-Testing
*/
//#undef int() // fix for braindead cast macros in some Wiring versions (0011?) that break stdlib
// (...avr/include/stdio.h:xxx: error: expected unqualified-id before 'int'...)
// testing in 0012, forcing that WProgram.h (includes stdlib) always comes before WConstants.h seems to fix it...

#include "Blinken.h"




// initialize with data pin only (supply own power & ground) <-- RECOMMENDED!
Blinken::Blinken(int dataPin)
{
  _blinken_dataPin=dataPin;
  digitalWrite(_blinken_dataPin, LOW);
  pinMode(_blinken_dataPin, OUTPUT);
}

// For very small number of devices (1?) Arduino I/O pins can supply power, data & ground
Blinken::Blinken(int powerPin, int dataPin, int groundPin)
{
  _blinken_dataPin=dataPin;
  _blinken_groundPin=groundPin;
  _blinken_powerPin=powerPin;

  digitalWrite(_blinken_dataPin, LOW);
  digitalWrite(_blinken_groundPin, LOW);
  digitalWrite(_blinken_powerPin, HIGH);
  pinMode(_blinken_groundPin, OUTPUT);
  pinMode(_blinken_dataPin, OUTPUT);
  pinMode(_blinken_powerPin, OUTPUT);
}




/* -------------------------- USER FUNCTIONS ------------------------------------------ */

// Reset all devices on the bus by sending an intentionally broken (short) cmd packet; the simplest way is a START with no data.
// In the case of an incomplete cmd (bits received are not a multiple of 16), Blinkens will self-reset in approx. 18ms.
// Delay a small amount after the reset since a freshly reset Blinken waits for a "quiet period" to begin listening in order to ensure
// it did not start listening in the middle of a bitstream.
void Blinken::reset()
{
  stop(); // quiet period
  start();   // START condition
  stop();    // quiet period
  delay(500);   // ...
}

/* Set the color of device 'addr' to RGB value (r,g,b). Valid values of (r,g,b) are 0 through 8. */
void Blinken::set_color(byte addr, byte r, byte g, byte b)
{
   byte cmd;
   r = r & 0x0F; /* Technically these should be constrained to 8 or less, but this is quick to do with a bit mask.*/
   g = g & 0x0F; /* In v1.3, sending an invalid value could cause undesired operation. */
   b = b & 0x0F;

   /* There are 4 possible conditions where 2 or more of the colors passed in match. In these cases we can save bus commands by */
   /* setting multiple color flags in a single cmd. Remember Blinkenlichten packet is <addr><0RGBiiii> where the R,G,B flags specify */
   /* the colors this cmd applies to. */
   /* Bus commands will be much more 'expensive', timewise, than the checks below. */
   if (r==g)
   {
      if(g==b) /* rgb */
      {
         cmd = (CMD_COLOR_R | CMD_COLOR_G | CMD_COLOR_B | r);
         writeraw(addr, cmd);
      }
      else /* rg */
      {
         cmd = (CMD_COLOR_R | CMD_COLOR_G | r);
         writeraw(addr, cmd);
         cmd = (CMD_COLOR_B | b);
         writeraw(addr, cmd);
      }
   }
   else if (r==b) /* rb */
   {
      cmd = (CMD_COLOR_R | CMD_COLOR_B | r);
      writeraw(addr, cmd);
      cmd = (CMD_COLOR_G | g);
      writeraw(addr, cmd);
   }
   else
   {
      cmd = (CMD_COLOR_R | r);
      writeraw(addr, cmd);
      cmd = (CMD_COLOR_G | g);
      writeraw(addr, cmd);
      cmd = (CMD_COLOR_B | b);
      writeraw(addr, cmd);
   }
}


/* Assign the node 'addr' to group 'group_addr'. The range of assignable group addresses is 1 ~ 63. Setting a device's group address to 0 effectively ungroups it (since all devices listen on 0 anyway). */
void Blinken::set_group(byte addr, byte group_addr)
{
   group_addr = group_addr & 0x3F;
   writeraw(addr, CMD_SET_GROUP_ADDR | group_addr);
}

/* Creature comfort wrapper function: equivalent to set_group(0,0) */
void Blinken::clear_all_groups()
{
   writeraw((byte)0, CMD_SET_GROUP_ADDR | 0); /* '0' set group '0' */
}

/* Send a deferred color value update. It will not be shown until the next 'activate_deferred()'. */
void Blinken::set_color_deferred(byte addr, byte r, byte g, byte b)
{
   /* Unlike the usual 'set color', here we cannot combine colors into a single command. */
   writeraw(addr, CMD_DEFER_R | (r & 0x0F));
   writeraw(addr, CMD_DEFER_G | (g & 0x0F));
   writeraw(addr, CMD_DEFER_B | (b & 0x0F));
}

/* Identify whether a device with address (or group address) 'addr' exists on the bus. Returns TRUE if so, FALSE if not. */
boolean Blinken::identify(byte addr)
{
   int gotResponse; /* why did they make this an int?? */
   /* FIXME: Test me! */
   writeraw(addr, CMD_IDENTIFY);
   /* NOTE: Want to delay about 100uS here to wait for the device to respond. Currently, I'm just counting on the STOP function */
   /* to provide this delay. If that value changes, this function may need to be modified. */
   pinMode(_blinken_dataPin, INPUT); /* Switch this pin to an input to read the response */
   delayMicroseconds(50);   /* Small additional delay to ensure data line's contents have time to settle */
   gotResponse=digitalRead(_blinken_dataPin); /* Read the pin. If '1', a devcie is responding. */

   digitalWrite(_blinken_dataPin, gotResponse); /* needed? */

   pinMode(_blinken_dataPin, OUTPUT); /* Hopefully this retains the value which was read, but no guarantees... */

   /* The icky bit: If a device responded, any other devices on the bus will see this long HIGH pulse as a START condition, and */
   /* try to receive a subsequent command. So, if a device responded, send a dummy command to eat the response into its own START */
   /* and send everyone the bytes they are now expecting. */
   if (gotResponse != 0)
   {
      /* If someone responded, they will pull-up the line for a fixed time (approx. 1ms), then go back to receiving commands.*/
      /* Thus to avoid unsightly reset flash of that device too, make sure the dummy START is long enough (>>1ms) */
      /* that the responding device sees it too. Some Arduinos and clones have non-standard clock speeds, so err on the side of caution. */
      /* The allowed window is >1ms and <18ms, since blinkens reset if a cmd doesn't complete in that time. */
      delay(3);
      writeraw((byte)0, CMD_NO_OP);
      delay(1);
      return true;
   }
   delay(1);
   return false;
}

/* Display any Deferred Updates. Useful if you have a slow controller and/or large number of devices, and want them all to change at the same time. */
void Blinken::activate_deferred()
{
   writeraw((byte)0, CMD_ACTIVATE_DEFERRED);
}

/* Display any Deferred Updates only for a specific device (group). */
void Blinken::activate_deferred(byte addr)
{
   writeraw(addr, CMD_ACTIVATE_DEFERRED);
}

/* Extinguish all devices and enter Power Save mode. They will re-awaken when the next cmd is sent. */
void Blinken::power_save()
{
   writeraw((byte)0, CMD_COLOR_W | 0); /* Send intensity of 0 for all colors of all devices */
   writeraw((byte)0, CMD_POWER_SAVE); /* Stop all device clocks; average power consumption per device reduced to ~ <1uA */
}

/* -------------------------- INTERNAL FUNCTIONS ------------------------------------------ */

/* Support function: Send raw START condition */
void Blinken::start()
{
  //digitalWrite(_blinken_dataPin, LOW);
  //delay(100);
  digitalWrite(_blinken_dataPin, HIGH);
  delayMicroseconds(100);
}

/* Support function: Send raw STOP condition */
void Blinken::stop()
{
  digitalWrite(_blinken_dataPin, LOW);
  delayMicroseconds(200);
}

/* Support function: Send raw '0' bit */
void Blinken::databitzero()
{
  digitalWrite(_blinken_dataPin, LOW);
  delayMicroseconds(20);
  digitalWrite(_blinken_dataPin, HIGH);
  delayMicroseconds(40);
}

/* Support function: Send raw '1' bit */
void Blinken::databitone()
{
  digitalWrite(_blinken_dataPin, LOW);
  delayMicroseconds(40);
  digitalWrite(_blinken_dataPin, HIGH);
  delayMicroseconds(20);
}

/* Support function: Send raw byte on the bus */
void Blinken::writebits(byte n)
{
   for(byte x=7; x!=0xFF; x--)  // Does not like "x>=0" construct - loops infinitely...
   {
      if(BIT_TEST(n, x)) {
         databitone();
      } else {
         databitzero();
      }
   }
}

/* Support function: Send raw data packet of format <start><addr><data><stop>. Most users should not use this function directly, but one of the many 'User' functions that wrap it nicely. */
void Blinken::writeraw(byte addr, byte data)
{
   /* On a suitably slow processor, the separation between byte write loops here could introduce a noticeable stretch of the last half of the last addr bit*/
   /* Ideally, would want to unwrap both writebits(...) loops and just inline it here to speed things up a bit. But what a mess...*/

   start();
   writebits(addr);
   writebits(data);
   stop();
}

