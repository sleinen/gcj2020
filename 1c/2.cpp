#include <iostream>
#include <cstdint>
#include <algorithm>
#include <vector>

using namespace std;

static bool desc (uint32_t x, uint32_t y) { return y < x; }

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
  cin >> T;
  for (unsigned caseno = 1; caseno <= T; ++caseno) {
    uint64_t seen = 0;
    uint32_t counts[26] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
    cin >> U;
    char result[11];
    result[10] = 0;
    for (k = 0; k < 10000; ++k) {
      cin >> Qi >> Ri;
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
    std::vector<uint32_t> countvec(counts, counts+26);
    sort(countvec.begin(), countvec.end(), desc);
    vector<uint32_t>::iterator it;
    for (it=countvec.begin(), i = 0; it!=countvec.begin()+9; ++it, ++i)
      result[i+1] = pos(*it, counts, 26) + 'A';
    cout << "Case #" << caseno << ": " << result << "\n";
  }
}
