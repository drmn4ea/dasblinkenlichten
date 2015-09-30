
#ifndef blinken_h
#define blinken_h

#if (ARDUINO >= 100)
 #include "Arduino.h"
#else
 #include "WProgram.h"
 //#include "WConstants.h"
#endif



#define BIT_MASK(x) (1 << x)
#define BIT_TEST(n,x) (n & BIT_MASK(x))
#define COLOR_R (0x4)
#define COLOR_G (0x2)
#define COLOR_B (0x1)
#define COLOR_W (COLOR_R | COLOR_G | COLOR_B)

/* Here color and other cmd bits are defined, relative to their position in a full command byte. */
/* I couldn't find it documented whether Wiring supports any kind of binary syntax (e.g. 0b01010101), so these are left as ugly hex. */
#define CMD_COLOR_R (0x40)
#define CMD_COLOR_G (0x20)
#define CMD_COLOR_B (0x10)
#define CMD_COLOR_W (CMD_COLOR_R | CMD_COLOR_G | CMD_COLOR_B)

#define CMD_EXTCMD_RAW (0x80) /* Raw Extended command <1xxxxxxx>*/
#define CMD_SET_GROUP_ADDR (0xC0) /* <11xxxxxx> */
#define CMD_DEFER_R (0xC1) /* <1001xxxx> */
#define CMD_DEFER_G (0xC2) /* <1010xxxx> */
#define CMD_DEFER_B (0xC3) /* <1011xxxx> */
#define CMD_NO_OP (0x88) /* <10001000> This cmd doesn't do anything. On purpose. */
#define CMD_IDENTIFY (0x84) /* <10000100> */
#define CMD_ACTIVATE_DEFERRED (0x82) /* <10000010> */
#define CMD_POWER_SAVE (0x81) /* <10000001> */



class Blinken
{
 public:
        Blinken(int dataPin);
        Blinken(int powerPin, int dataPin, int groundPin);
        void reset();
        void start();
        void stop();
        void databitzero();
        void databitone();
        void writebits(byte n);
        void writeraw(byte addr, byte data);
        void set_color(byte addr, byte r, byte g, byte b);
        void set_group(byte addr, byte group_addr);
        void clear_all_groups();
        void set_color_deferred(byte addr, byte r, byte g, byte b);
        boolean identify(byte addr);
        void activate_deferred();
        void activate_deferred(byte addr);
        void power_save();
 private:
         int _blinken_dataPin;
         int _blinken_powerPin;
         int _blinken_groundPin;
};



#endif