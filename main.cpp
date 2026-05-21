#include <iostream>
#include <fstream>
#include <map>
#include <vector>
#include <string>
#include <cmath>
#include <numeric>
#include <algorithm>

using Path = std::string;

struct FaAdat {
    std::vector<int> levelMelysegek;
    int maxMelyseg = 0;
};

class LZWFa {
    std::map<Path, bool> csomok;
    Path aktualis;

public:
    void operator<<(char b) {
        aktualis += b;
        if (csomok.find(aktualis) == csomok.end()) {
            csomok[aktualis] = true;
            aktualis.clear();
        }
    }

    void kiir(std::ostream& os) const {
        std::vector<Path> utak;
        for (const auto& [p, _] : csomok)
            utak.push_back(p);
        std::sort(utak.begin(), utak.end(), [](const Path& a, const Path& b) {
            if (a.size() != b.size()) return a.size() < b.size();
            return a > b;
        });
        for (const auto& p : utak) {
            int m = static_cast<int>(p.size());
            for (int i = 0; i < m; ++i) os << "---";
            os << p.back() << '(' << m - 1 << ")\n";
        }
        os << "---/(" << 0 << ")\n";
    }

    FaAdat statisztikak() const {
        FaAdat adat;
        for (const auto& [p, _] : csomok) {
            int m = static_cast<int>(p.size());
            bool level =
                csomok.find(p + '0') == csomok.end() &&
                csomok.find(p + '1') == csomok.end();
            if (level) {
                adat.levelMelysegek.push_back(m);
                if (m > adat.maxMelyseg) adat.maxMelyseg = m;
            }
        }
        return adat;
    }

    friend std::ostream& operator<<(std::ostream& os, const LZWFa& f) {
        f.kiir(os); return os;
    }
};

static double atlag(const std::vector<int>& v) {
    if (v.empty()) return 0.0;
    return static_cast<double>(std::accumulate(v.begin(), v.end(), 0)) / v.size();
}

static double szoras(const std::vector<int>& v, double atl) {
    if (v.size() <= 1) return 0.0;
    double ossz = 0.0;
    for (int x : v) ossz += (x - atl) * (x - atl);
    return std::sqrt(ossz / (v.size() - 1));
}

static void usage(const char* prog) {
    std::cout << "Hasznalat: " << prog << " bemenet -o kimenet\n";
}

int main(int argc, char* argv[]) {
    if (argc != 4) { usage(argv[0]); return -1; }
    if (std::string(argv[2]) != "-o") { usage(argv[0]); return -2; }

    std::ifstream be(argv[1], std::ios::binary);
    if (!be) { std::cerr << argv[1] << " nem letezik\n"; return -3; }
    std::ofstream ki(argv[3]);

    for (unsigned char b; be.read(reinterpret_cast<char*>(&b), 1) && b != '\n';) {}

    LZWFa fa;
    bool komment = false;
    unsigned char b;

    while (be.read(reinterpret_cast<char*>(&b), 1)) {
        if (b == '>') { komment = true;  continue; }
        if (b == '\n') { komment = false; continue; }
        if (komment || b == 'N') continue;
        for (int i = 7; i >= 0; --i)
            fa << ((b >> i & 1) ? '1' : '0');
    }

    ki << fa;
    auto adat = fa.statisztikak();
    double atl = atlag(adat.levelMelysegek);
    ki << "depth = " << adat.maxMelyseg                  << '\n';
    ki << "mean = "  << atl                               << '\n';
    ki << "var = "   << szoras(adat.levelMelysegek, atl) << '\n';
}
