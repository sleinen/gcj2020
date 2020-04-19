/*
 qr1.c

 Date Created: Sat Apr  4 12:20:38 2020
 Author:       Simon Leinen  <simon.leinen@switch.ch>
 */
#include <iostream>
#include <cstdint>

using namespace std;

const unsigned maxN = 100;

int
main () {
  unsigned T, N;
  cin >> T;
  for (unsigned caseno = 1; caseno <= T; ++caseno) {
    unsigned A[maxN][maxN];
    uint8_t dup_rows = 0;
    uint8_t dup_cols = 0;
    cin >> N;
    for (int i = 0; i < N; i++) {
      for (int j = 0; j < N; j++) {
        cin >> A[i][j];
      }
    }
    unsigned trace = 0;
    for (int a = 0; a < N; a++) {
      trace += A[a][a];
      uint8_t row_seen[maxN], this_row_dup = 0;
      uint8_t col_seen[maxN], this_col_dup = 0;
      for (int b = 0; b < N; b++) {
        row_seen[b] = col_seen[b] = 0;
      }
      for (int b = 0; b < N; b++) {
        unsigned c1 = A[a][b] - 1;
        unsigned c2 = A[b][a] - 1;
        if (row_seen[c1]) {
          if (!this_row_dup) { this_row_dup = 1; dup_rows++; };
        } else {
          row_seen[c1] = 1;
        }
        if (col_seen[c2]) {
          if (!this_col_dup) { this_col_dup = 1; dup_cols++; };
        } else {
          col_seen[c2] = 1;
        }
      }
    }
    cout << "Case #" << caseno << ": " << trace << " " << (int) dup_rows << " " << (int) dup_cols;
    cout << "\n";
  }
}
