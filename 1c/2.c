#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <memory.h>

static int _desc (const uint32_t *x, const uint32_t *y) { return *y - *x; }
static int desc (const void *x, const void *y) { return _desc(x, y); }

static int pos(uint32_t val, uint32_t *v, size_t count) {
  for (int k = 0; k < count; ++k) { if (v[k] == val) return k; }
  return -1;
}

int
main () {
  unsigned T, U;
  int64_t Qi;
  char Ri[100], *cp;
  unsigned i, k;
  scanf ("%u\n", &T);
  for (unsigned caseno = 1; caseno <= T; ++caseno) {
    uint64_t seen = 0;
    uint32_t counts[26] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
    scanf ("%u\n", &U);
    char result[11];
    result[10] = 0;
    for (k = 0; k < 10000; ++k) {
      scanf ("%lld %s\n", &Qi, Ri);
      for (i = 0, cp = Ri; *cp; ++i, ++cp) {
        if (i == 0) {
          ++counts[*cp-'A'];
        };
        seen |= (1 << (*cp-'A'));
      }
    }
    for (k = 0; k < 26; ++k) {
      if ((seen & (1 << k)) && counts[k] == 0) {
        result[0] = k + 'A';
      }
    }
    uint32_t scounts[26], *it;
    memcpy (scounts, counts, sizeof (uint32_t) * 26);
    qsort (scounts, 26, sizeof (uint32_t), desc);
    for (it = scounts, i = 0; i < 9; ++it, ++i)
      result[i+1] = pos(*it, counts, 26) + 'A';
    printf ("Case #%d: %s\n", caseno, result);
  }
}
