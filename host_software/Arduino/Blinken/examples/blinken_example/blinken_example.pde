/*

Quick n dirty Blinken demo for Arduino + clones
Identifies all Blinkens on the bus, then cycles them through different colors one at a time.

*/

#include <Blinken.h>


int dataPin=12;
int powerPin=5;
int groundPin=7;

Blinken blinken(powerPin, dataPin, groundPin);

float rad;

byte blinkens[255]; // preallocate an array to hold a list of Blinkens (IDs) on the bus
byte nBlinkens=0;  // how many were discovered

//byte fadecolors[40]; // i*

void setup()
{
  blinken.reset(); // Make sure if the Adruino was powered but not active (e.g. during code upload) and pins may have been floating, everyone is back in sync.
  Serial.begin(9600);
  Serial.println("Test of identify function...");
  delay(500);
  for (byte i=1; i<0xFF; i++)    // "i<=0xFF" creates an infinite loop, wtf?
  {
    if(blinken.identify(i)) // check if anyone responds to this address
    {
      Serial.print("Device ");
      Serial.print(i, HEX);
      Serial.print(" responded!\n");
      blinkens[nBlinkens]=i;    // if something responded, add its ID to the list of active IDs
      nBlinkens++; // and update the total count. blinkens[0 .. nBlinkens-1] are valid IDs.
    }
  }
}


void loop()
{
  // First loop - test each device in turn (R, G, B, psychedelic)
  for (int which=0; which < nBlinkens; which++)
  {
    blinken.set_color(blinkens[which], 8, 0, 0); // R
    delay(250);
    blinken.set_color(blinkens[which], 0, 8, 0); // G
    delay(250);
    blinken.set_color(blinkens[which], 0, 0, 8); // B
    delay(250);
    for(int i=0; i<180; i++)  // Psychedelic
    {
      rad=i*(3.14159/180); // bah, I think in degrees...
      blinken.set_color(blinkens[which], abs(int(sin(rad)*8)), abs(int(cos(rad)*8)), abs(int(sin(rad)*8)-7));
    }
    blinken.set_color(blinkens[which], 0, 0, 0); // Extinguish...
  }
  delay(250);

  // Everyone psychedelic
  for (int i=0; i<5; i++)
  {
    for(int i=0; i<180; i++)  // Psychedelic
    {
      rad=i*(3.14159/180); // bah, I think in degrees...
      blinken.set_color(0, abs(int(sin(rad)*8)), abs(int(cos(rad)*8)), abs(int(sin(rad)*8)-7));
    }
    blinken.set_color(0, 0, 0, 0); // Extinguish...
  }


  // next - play with the random number generator...
  for (int i=0; i<8; i++)
  {
    for (int which=0; which < nBlinkens; which++)
    {
      blinken.set_color(blinkens[which],random(0,9), random(0,9), random(0,9));
    }
    delay(500);
  }
  blinken.set_color(0,0,0,0);
  
  // spreading colors...
  for (int i=0; i<8; i++)
  {
    byte r=random(0,9);
    byte g=random(0,9);
    byte b=random(0,9);
    for (int which=0; which < nBlinkens; which++)
    {
      blinken.set_color(blinkens[which], r, g, b);
      delay(200);
    }
    for (int which=0; which < nBlinkens; which++)
    {
      blinken.set_color(blinkens[which], 0, 0, 0);
      delay(200);
    }    
  }
  blinken.set_color(0,0,0,0);
  delay(100);
  
    // spreading colors, speedtest...
  for (int i=0; i<256; i++)
  {
    for (int which=0; which < nBlinkens; which++)
    {
      blinken.set_color(blinkens[which], random(0,9), random(0,9), random(0,9));
      //delay(100);
    }
  }
  blinken.set_color(0,0,0,0);
}
