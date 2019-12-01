/* MD5
 converted to C++ class by Frank Thilo (thilo@unix-ag.org)
 for bzflag (http://www.bzflag.org)
 
   based on:
 
   md5.h and md5.c
   reference implemantion of RFC 1321
 
   Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
rights reserved.
 
License to copy and use this software is granted provided that it
is identified as the "RSA Data Security, Inc. MD5 Message-Digest
Algorithm" in all material mentioning or referencing this software
or this function.
 
License is also granted to make and use derivative works provided
that such works are identified as "derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm" in all material
mentioning or referencing the derived work.
 
RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided "as is"
without express or implied warranty of any kind.
 
These notices must be retained in any copies of any part of this
documentation and/or software.
 
*/
 
/* interface header */
#include "md5.h"
 
/* system implementation headers */
#include <cstdio>
#include <cstring>
 
#define USE_BOOST
#ifdef USE_BOOST
#include <boost/endian/conversion.hpp> 
#endif

// Constants for MD5Transform routine.
#define S11 7
#define S12 12
#define S13 17
#define S14 22
#define S21 5
#define S22 9
#define S23 14
#define S24 20
#define S31 4
#define S32 11
#define S33 16
#define S34 23
#define S41 6
#define S42 10
#define S43 15
#define S44 21
 
///////////////////////////////////////////////
 
// F, G, H and I are basic MD5 functions.
static inline constexpr std::uint32_t F(std::uint32_t x, std::uint32_t y, std::uint32_t z) {
  return (x & y) | (~x & z);
}
 
static inline constexpr std::uint32_t G(std::uint32_t x, std::uint32_t y, std::uint32_t z) {
  return (x & z) | (y & ~z);
}
 
static inline constexpr std::uint32_t H(std::uint32_t x, std::uint32_t y, std::uint32_t z) {
  return x ^ y ^ z;
}
 
static inline constexpr std::uint32_t I(std::uint32_t x, std::uint32_t y, std::uint32_t z) {
  return y ^ (x | ~z);
}
 
// rotate_left rotates x left n bits.
static inline constexpr std::uint32_t rotate_left(std::uint32_t x, int n) {
  return (x << n) | (x >> (32 - n));
}
 
// FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
// Rotation is separate from addition to prevent recomputation.
static inline constexpr std::uint32_t FF(std::uint32_t a, std::uint32_t b, std::uint32_t c, std::uint32_t d, std::uint32_t x, std::uint32_t s, std::uint32_t ac) {
  return rotate_left(a + F(b,c,d) + x + ac, s) + b;
}
 
static inline constexpr std::uint32_t GG(std::uint32_t a, std::uint32_t b, std::uint32_t c, std::uint32_t d, std::uint32_t x, std::uint32_t s, std::uint32_t ac) {
  return rotate_left(a + G(b,c,d) + x + ac, s) + b;
}
 
static inline constexpr std::uint32_t HH(std::uint32_t a, std::uint32_t b, std::uint32_t c, std::uint32_t d, std::uint32_t x, std::uint32_t s, std::uint32_t ac) {
  return rotate_left(a + H(b,c,d) + x + ac, s) + b;
}
 
static inline constexpr std::uint32_t II(std::uint32_t a, std::uint32_t b, std::uint32_t c, std::uint32_t d, std::uint32_t x, std::uint32_t s, std::uint32_t ac) {
  return rotate_left(a + I(b,c,d) + x + ac, s) + b;
}
 
//////////////////////////////////////////////
 
// default ctor, just initalize
MD5::MD5() : finalized(false)
{
	count[0] = 0;
	count[1] = 0;
	
	state[0] = 0x67452301;
	state[1] = 0xefcdab89;
  state[2] = 0x98badcfe;
  state[3] = 0x10325476;
}
 
//////////////////////////////////////////////
 
// nifty shortcut ctor, compute MD5 for string and finalize it right away
MD5::MD5(const std::string &text) : MD5()
{
  update(text);
  finalize();
}
 
MD5::MD5(const char *text) noexcept : MD5()
{
	auto len = std::strlen(text);
	update(reinterpret_cast<const uint8_t *>(text), len);
	finalize();
}

//////////////////////////////
 
// decodes input (unsigned char) into output (std::uint32_t). Assumes len is a multiple of 4.
void
MD5::decode(std::uint32_t output[], const std::uint8_t input[], size_type len) noexcept
{
  for (unsigned int i = 0, j = 0; j < len; i++, j += 4) {
#ifdef USE_BOOST
		std::uint32_t word;
		std::memcpy(&word, input + j, 4);
		output[i] = boost::endian::little_to_native(word);
#else
    output[i] = input[j] | (input[j+1] << 8) |
      (input[j+2] << 16) | (input[j+3] << 24);
#endif
  }
}
 
//////////////////////////////
 
// encodes input (std::uint32_t) into output (unsigned char). Assumes len is
// a multiple of 4.
void
MD5::encode(std::uint8_t output[], const std::uint32_t input[], size_type len) noexcept
{
  for (size_type i = 0, j = 0; j < len; i++, j += 4) {
#ifdef USE_BOOST
		std::uint32_t le = boost::endian::native_to_little(input[i]);
		std::memcpy(output + j, &le, 4);
#else
    output[j] = input[i] & 0xff;
    output[j+1] = (input[i] >> 8) & 0xff;
    output[j+2] = (input[i] >> 16) & 0xff;
    output[j+3] = (input[i] >> 24) & 0xff;
#endif
  }
}
 
//////////////////////////////
 
// apply MD5 algo on a block
void
MD5::transform(const std::uint8_t block[blocksize]) noexcept
{
  std::uint32_t a = state[0], b = state[1], c = state[2], d = state[3], x[16];
  decode (x, block, blocksize);
 
  /* Round 1 */
  a = FF (a, b, c, d, x[ 0], S11, 0xd76aa478); /* 1 */
  d = FF (d, a, b, c, x[ 1], S12, 0xe8c7b756); /* 2 */
  c = FF (c, d, a, b, x[ 2], S13, 0x242070db); /* 3 */
  b = FF (b, c, d, a, x[ 3], S14, 0xc1bdceee); /* 4 */
  a = FF (a, b, c, d, x[ 4], S11, 0xf57c0faf); /* 5 */
  d = FF (d, a, b, c, x[ 5], S12, 0x4787c62a); /* 6 */
  c = FF (c, d, a, b, x[ 6], S13, 0xa8304613); /* 7 */
  b = FF (b, c, d, a, x[ 7], S14, 0xfd469501); /* 8 */
  a = FF (a, b, c, d, x[ 8], S11, 0x698098d8); /* 9 */
  d = FF (d, a, b, c, x[ 9], S12, 0x8b44f7af); /* 10 */
  c = FF (c, d, a, b, x[10], S13, 0xffff5bb1); /* 11 */
  b = FF (b, c, d, a, x[11], S14, 0x895cd7be); /* 12 */
  a = FF (a, b, c, d, x[12], S11, 0x6b901122); /* 13 */
  d = FF (d, a, b, c, x[13], S12, 0xfd987193); /* 14 */
  c = FF (c, d, a, b, x[14], S13, 0xa679438e); /* 15 */
  b = FF (b, c, d, a, x[15], S14, 0x49b40821); /* 16 */
 
  /* Round 2 */
  a = GG (a, b, c, d, x[ 1], S21, 0xf61e2562); /* 17 */
  d = GG (d, a, b, c, x[ 6], S22, 0xc040b340); /* 18 */
  c = GG (c, d, a, b, x[11], S23, 0x265e5a51); /* 19 */
  b = GG (b, c, d, a, x[ 0], S24, 0xe9b6c7aa); /* 20 */
  a = GG (a, b, c, d, x[ 5], S21, 0xd62f105d); /* 21 */
  d = GG (d, a, b, c, x[10], S22,  0x2441453); /* 22 */
  c = GG (c, d, a, b, x[15], S23, 0xd8a1e681); /* 23 */
  b = GG (b, c, d, a, x[ 4], S24, 0xe7d3fbc8); /* 24 */
  a = GG (a, b, c, d, x[ 9], S21, 0x21e1cde6); /* 25 */
  d = GG (d, a, b, c, x[14], S22, 0xc33707d6); /* 26 */
  c = GG (c, d, a, b, x[ 3], S23, 0xf4d50d87); /* 27 */
  b = GG (b, c, d, a, x[ 8], S24, 0x455a14ed); /* 28 */
  a = GG (a, b, c, d, x[13], S21, 0xa9e3e905); /* 29 */
  d = GG (d, a, b, c, x[ 2], S22, 0xfcefa3f8); /* 30 */
  c = GG (c, d, a, b, x[ 7], S23, 0x676f02d9); /* 31 */
  b = GG (b, c, d, a, x[12], S24, 0x8d2a4c8a); /* 32 */
 
  /* Round 3 */
  a = HH (a, b, c, d, x[ 5], S31, 0xfffa3942); /* 33 */
  d = HH (d, a, b, c, x[ 8], S32, 0x8771f681); /* 34 */
  c = HH (c, d, a, b, x[11], S33, 0x6d9d6122); /* 35 */
  b = HH (b, c, d, a, x[14], S34, 0xfde5380c); /* 36 */
  a = HH (a, b, c, d, x[ 1], S31, 0xa4beea44); /* 37 */
  d = HH (d, a, b, c, x[ 4], S32, 0x4bdecfa9); /* 38 */
  c = HH (c, d, a, b, x[ 7], S33, 0xf6bb4b60); /* 39 */
  b = HH (b, c, d, a, x[10], S34, 0xbebfbc70); /* 40 */
  a = HH (a, b, c, d, x[13], S31, 0x289b7ec6); /* 41 */
  d = HH (d, a, b, c, x[ 0], S32, 0xeaa127fa); /* 42 */
  c = HH (c, d, a, b, x[ 3], S33, 0xd4ef3085); /* 43 */
  b = HH (b, c, d, a, x[ 6], S34,  0x4881d05); /* 44 */
  a = HH (a, b, c, d, x[ 9], S31, 0xd9d4d039); /* 45 */
  d = HH (d, a, b, c, x[12], S32, 0xe6db99e5); /* 46 */
  c = HH (c, d, a, b, x[15], S33, 0x1fa27cf8); /* 47 */
  b = HH (b, c, d, a, x[ 2], S34, 0xc4ac5665); /* 48 */
 
  /* Round 4 */
  a = II (a, b, c, d, x[ 0], S41, 0xf4292244); /* 49 */
  d = II (d, a, b, c, x[ 7], S42, 0x432aff97); /* 50 */
  c = II (c, d, a, b, x[14], S43, 0xab9423a7); /* 51 */
  b = II (b, c, d, a, x[ 5], S44, 0xfc93a039); /* 52 */
  a = II (a, b, c, d, x[12], S41, 0x655b59c3); /* 53 */
  d = II (d, a, b, c, x[ 3], S42, 0x8f0ccc92); /* 54 */
  c = II (c, d, a, b, x[10], S43, 0xffeff47d); /* 55 */
  b = II (b, c, d, a, x[ 1], S44, 0x85845dd1); /* 56 */
  a = II (a, b, c, d, x[ 8], S41, 0x6fa87e4f); /* 57 */
  d = II (d, a, b, c, x[15], S42, 0xfe2ce6e0); /* 58 */
  c = II (c, d, a, b, x[ 6], S43, 0xa3014314); /* 59 */
  b = II (b, c, d, a, x[13], S44, 0x4e0811a1); /* 60 */
  a = II (a, b, c, d, x[ 4], S41, 0xf7537e82); /* 61 */
  d = II (d, a, b, c, x[11], S42, 0xbd3af235); /* 62 */
  c = II (c, d, a, b, x[ 2], S43, 0x2ad7d2bb); /* 63 */
  b = II (b, c, d, a, x[ 9], S44, 0xeb86d391); /* 64 */
 
  state[0] += a;
  state[1] += b;
  state[2] += c;
  state[3] += d;
 
  // Zeroize sensitive information.
  std::memset(x, 0, sizeof x);
}
 
//////////////////////////////
 
// MD5 block update operation. Continues an MD5 message-digest
// operation, processing another message block
void
MD5::update(const std::uint8_t input[], size_type length) noexcept
{
  // compute number of bytes mod 64
  size_type index = count[0] / 8 % blocksize;
 
  // Update number of bits
  if ((count[0] += (length << 3)) < (length << 3))
    count[1]++;
  count[1] += (length >> 29);
 
  // number of bytes we need to fill in buffer
  size_type firstpart = 64 - index;
 
  size_type i;
 
  // transform as many times as possible.
  if (length >= firstpart)
  {
    // fill buffer first, transform
    std::memcpy(&buffer[index], input, firstpart);
    transform(buffer);
 
    // transform chunks of blocksize (64 bytes)
    for (i = firstpart; i + blocksize <= length; i += blocksize)
      transform(&input[i]);
 
    index = 0;
  }
  else
    i = 0;
 
  // buffer remaining input
  std::memcpy(&buffer[index], &input[i], length-i);
}
 
//////////////////////////////
 
// for convenience provide a verson with signed char
void
MD5::update(const char input[], size_type length) noexcept
{
  update(reinterpret_cast<const std::uint8_t*>(input), length);
}

void
MD5::update(const std::string &input) noexcept
{
	update(reinterpret_cast<const std::uint8_t*>(input.data()), input.length());
}

//////////////////////////////
 
// MD5 finalization. Ends an MD5 message-digest operation, writing the
// the message digest and zeroizing the context.
MD5&
MD5::finalize() noexcept
{
  static std::uint8_t padding[64] = {
    0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  };
 
  if (!finalized) {
    // Save number of bits
    std::uint8_t bits[8];
    encode(bits, count, 8);
 
    // pad out to 56 mod 64.
    size_type index = count[0] / 8 % 64;
    size_type padLen = (index < 56) ? (56 - index) : (120 - index);
    update(padding, padLen);
 
    // Append length (before padding)
    update(bits, 8);
 
    // Store state in digest
    encode(digest, state, 16);
 
    // Zeroize sensitive information.
    std::memset(buffer, 0, sizeof buffer);
    std::memset(count, 0, sizeof count);
 
    finalized=true;
  }
 
  return *this;
}
 
//////////////////////////////
 
// return hex representation of digest as string
	
const char *
MD5::hexdigest_cstr() const noexcept
{
#ifdef _OPENMP
	static thread_local char buf[33];
#else
	static char buf[33];
#endif	
	
  if (!finalized) {
  	buf[0] = 0;
  	return buf;
  }
 
	static const char hexdigits[] = "0123456789abcdef";
	for (int i = 0; i < 16; i++) {
	  buf[i*2] = hexdigits[digest[i] >> 4];
	  buf[(i*2)+1] = hexdigits[digest[i] & 0x0F];
  }
  buf[32]=0;
 
  return buf;
}

std::string MD5::hexdigest() const 
{ 
  return std::string(hexdigest_cstr(), 32);
}
 
//////////////////////////////
 
std::ostream& operator<<(std::ostream& out, MD5 md5)
{
  return out << md5.hexdigest();
}
 
//////////////////////////////
 
std::string md5(const std::string &str)
{
    MD5 md5{str};
 
    return md5.hexdigest();
}

const char *
md5_cstr(const std::string &str)
{
	MD5 md5{str};
	return md5.hexdigest_cstr();
}