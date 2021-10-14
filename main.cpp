#include <cstdio>
#include <iostream>
#include <cmath>
#include <assert.h>
#include <vector>
#include <optional>
#include <string.h>

using namespace std;
using ll = long long;


template<typename T>
T clamp(int small, int cur, int large) {
  if (cur > large) { return large; }
  if (cur < small) { return small; }
    return cur; 
}

template<typename T>
std::ostream &operator << (std::ostream &o, const vector<T> &vs) {
  o << "[";
  for(int i = 0; i < vs.size(); ++i) {
    if (i > 0) { o << " "; } 
    o << vs[i];
  }
  return o << "]";
}


// L for location
struct Loc {
  ll si, line, col;
  Loc(ll si, ll line, ll col) : si(si), line(line), col(col){};
  Loc nextline() const { return Loc(si + 1, line + 1, 1); }
  static Loc beginning_of_file() { return Loc(0, 1, 1); }
  Loc next(char c) const {
    if (c == '\n') {
      return nextline();
    } else {
      return nextcol();
    }
  }

  Loc next(const char *s) const {
    Loc l = *this;
    for (int i = 0; s[i] != 0; ++i) {
      l = l.next(s[i]);
    }
    return l;
  }

  Loc prev(char c) const {
    if (c == '\n') {
      assert(false && "don't know how to walk back newline");
    } else {
      return prevcol();
    }
  }

  Loc prev(const char *s) const {
    Loc l = *this;
    for (int i = strlen(s) - 1; i >= 0; --i) {
      l = l.prev(s[i]);
    }
    return l;
  }

  bool operator==(const Loc &other) const {
    return si == other.si && line == other.line && col == other.col;
  }

  bool operator!=(const Loc &other) const { return !(*this == other); }

private:
  Loc nextcol() const { return Loc(si + 1, line, col + 1); }
  Loc prevcol() const {
    assert(col - 1 >= 1);
    return Loc(si - 1, line, col - 1);
  }
};
const Loc LOC_FIRST = Loc(0, 1, 1);

std::ostream &operator<<(std::ostream &o, const Loc &l) {
  return cout << ":" << l.line << ":" << l.col;
}


struct Tokenizer {
  int len;
  const char *data;
  Loc loc;
  

  Tokenizer(int len, const char *data)
      : len(len), data(data), loc(Loc::beginning_of_file()) {}

  bool eof() {
    return loc.si >= len;
  }

  void consume_line() {
    while(!eof()) {
      if (ispeek("\n")) {
        advance1(); break;
      } else {
        advance1();
      }
    }
  }

  string consumetok() {
    eat_whitespace();
    assert(!eof());
    string out;
    while(1) {
      if (loc.si >= len) { break; }
      char c = data[loc.si];
      if (c == '\n' ||  c == '\t' || c == ' ' || c == '/') { break; }
      out += c;
      loc = loc.next(c);
    }
    return out;
  };

  void consume(string s) {
    assert(ispeek(s));
    loc = loc.next(s.c_str());
  }


  void print_loc(Loc l) const {
    if (l.si >= this->len) {
      printf("\n%4lld>EOF", l.line);
      return;
    }
    cerr << "\n===\n";
    int i = l.si;
    for (; i >= 1 && data[i - 1] != '\n'; i--) {
    }

    printf("\n%4lld>", l.line);
    string squiggle;
    for (; data[i] != '\0' && data[i] != '\n'; ++i) {
      squiggle += i == l.si ? '^' : ' ';
      cerr << data[i];
    }
    printf("\n%4lld>%s\n", l.line, squiggle.c_str());
  }

  void print_current_loc() { this->print_loc(this->loc); }

private:
  bool ispeek(string  s) const {
    if (loc.si  + s.size() >= len) {
      return false;
    }

    for(int i = 0; i < s.size(); ++i) {
      if (data[loc.si + i] != s[i]) { return false; }
    }
    return true;
  }



  void advance1() {
    assert(!this->eof());
    loc = loc.next(data[this->loc.si]);
  }


  void eat_whitespace() {
    while (1) {
      if (ispeek(" ")) {
        consume(" ");
      } else if (ispeek("\t")) {
        consume("\t");
      } else if (ispeek("\n")) {
        consume("\n");
      } else {
        return;
      }
    }
  }
};




template<typename T>
struct Vec2 { 
  T x, y;
  Vec2() : x(0), y(0) {}
  Vec2(int x, int y) : x(x) , y(y) {}
  Vec2 abs() const {
    return Vec2(::abs(x), ::abs(y));
  }

    Vec2<T> operator - (Vec2<T> other) {
      return Vec2<T>(x - other.x, y - other.y);
    }
};
using Vec2i = Vec2<int>;

template<typename T>
ostream &operator << (ostream &o, Vec2<T> v) {
  return o << "v2(" << v.x << "  " << v.y << ")";
}


template<typename T>
struct Vec3T { 
  T x, y, z;
  Vec3T(T x, T y, T z): x(x), y(y), z(z) {}
};

template<typename T>
ostream &operator << (ostream &o, Vec3T<T> v) {
  return o << "v3(" << v.x << "  " << v.y << " " << v.z << ")";
}

using Vec3i = Vec3T<int>;
using Vec3f = Vec3T<float>;

struct Color {
  int r, g, b;
  Color() {
    // default is garish yellow.
    r = 255;
    g = 255;
    b = 0;
  }
  Color(int r, int g, int b)  : r(r), g(g), b(b) {}

  static Color white() {
    return Color(255, 255, 255);
  }

  static Color lightgray() {
    return Color(230, 230, 230);
  }

  static Color darkgray() {
    return Color(42, 42, 42);
  }
};

struct Image {
  int w, h;
  Color *arr;
  Image(int w, int h, Color cinit) : w(w), h(h), arr(new Color[w*h]) {
    for(int x = 0; x < w; ++x) {
      for(int y = 0; y < h; ++y) {
        this->arr[y*w+x] = cinit;
      }
    }
  }

  Color &operator ()(int x, int y) {
    assert(x >= 0);
    assert(y >= 0);
    assert(x < w);
    assert(y < h);
    return arr[y*w + x];
  }
};

// https://en.wikipedia.org/wiki/Netpbm#PPM_example
void write_image_to_ppm(Image &img, const char *fpath) {
  FILE *f = fopen(fpath, "wb");
  assert(f && "expected valid file path");
  fprintf(f, "%s\n", "P3");
  fprintf(f, "%d %d\n", img.w, img.h);
  // (0, 0) is bottom left in OUTPUT / PPM FILE
  // (0, 0) is top right in Image(x, y) / INPUT/ array img
  // we must write in FILE coordinates.
  for(int fy = 0; fy < img.h; ++fy) {
    for(int fx = 0; fx < img.w; ++fx) {
      const int iy = img.h - 1 - fy; // coordinate system change
      const int ix = fx;
      const Color c = img(ix, iy);
      fprintf(f, "%d %d %d\n", c.r , c.g, c.b);
    }
  }
  fclose(f);
}

struct Model {
    vector<Vec3f> verts;
    vector<vector<int>> faces;
};

Model parse_model(const char *file) {
  FILE *f = fopen(file, "rb");
  assert(f && "expected valid file path");

  fseek(f, 0, SEEK_END);
  const int filesize = ftell(f);
  fseek(f, 0, SEEK_SET);
  const char *buf = new char[filesize+1];
  int nread = fread((void *)buf, 1, filesize, f);   
  assert(nread == filesize);
  fclose(f);

  Tokenizer t(filesize, buf);
  Model model;
  while(!t.eof()) {
    std::string s = t.consumetok(); 
    cout << s << "\n";

    if (s == "v") {
      std::string x, y, z;
      x = t.consumetok();
      y = t.consumetok();
      z = t.consumetok();
      Vec3f vec = Vec3f(atof(x.c_str()), atof(y.c_str()), atof(z.c_str()));
      // cout << "vec: " << vec << "\n"; getchar();
      model.verts.push_back(vec);
    } else if (s == "f") {
      std::string trash;
      std::string fx = t.consumetok(); t.consume("/"); trash = t.consumetok(); t.consume("/"); t.consumetok();
      std::string fy = t.consumetok(); t.consume("/"); trash = t.consumetok(); t.consume("/"); t.consumetok();
      std::string fz = t.consumetok(); t.consume("/"); trash = t.consumetok(); t.consume("/"); t.consumetok();
      vector<int> face { atoi(fx.c_str())-1, atoi(fy.c_str())-1, atoi(fz.c_str())-1 };
      // cout << "face: " << face << "\n"; getchar();
      model.faces.push_back(face);
    } else {
      cout << "ignoring line: " << s << "\n";
      t.consume_line();
    }
  }
  return model;
};

void line(Vec2i begin, Vec2i end, Color color, Image &img) {
  cout << "line(" << begin << " " << end << ")";
  Vec2i d = (end - begin).abs();
  bool transposed = false;
  // line has slope > 1, make it slope < 1, so that we need more than 1 δx to see a δy
  if (d.x < d.y) {
    std::swap(begin.x, begin.y);
    std::swap(end.x, end.y);
    transposed = true;
  }

  if (begin.x > end.x) {
    std::swap(begin, end);
  }

  cout << " -> " << begin << " " << end << "\n";

  for(int x = begin.x; x <= end.x; x++) {
    const float t = (x - begin.x) / (float)(end.x - begin.x); // lerp value
    const int y = begin.y * (1.0 - t) + end.y * t;

    int outx = x;
    int outy = y;
    if (transposed) {
      std::swap(outx, outy);
    } 

    outx = clamp<int>(0, outx, img.w - 1);
    outy = clamp<int>(0, outy, img.h - 1);
    img(outx, outy) = color;
  }
}


void chapter1() {
// Lesson 1: https://github.com/ssloy/tinyrenderer/tree/f6fecb7ad493264ecd15e230411bfb1cca539a12
const int width  = 200;
const int height = 200;
  Model model = parse_model("./obj/african_head/african_head.obj");
  // Model model = parse_model("./obj/tri.obj");
  Image image(width, height, Color::darkgray());
  cout << "model has |" << model.verts.size() << "| vertices\n";
  cout << "model has |" << model.faces.size() << "| faces\n";
  for(int f = 0; f < model.faces.size(); ++f) {
    vector<int> face = model.faces[f];
    cout << "have face: " << face << " {";
    for(int j = 0; j < 3; ++j) {
      cout << model.verts[face[j]] << " ";
    }
    cout << "}\n";

    for(int j = 0; j < 3; ++j) {
      const Vec3f v0 = model.verts[face[j]];
      const Vec3f v1 = model.verts[face[(j+1)%3]];
      int x0 = (v0.x+1.)*width/2.;
      int y0 = (v0.y+1.)*height/2.;
      int x1 = (v1.x+1.)*width/2.;
      int y1 = (v1.y+1.)*height/2.;
      line(Vec2i(x0, y0), Vec2i(x1, y1), Color::white(), image);
    }
    cout << "\n";
  }

  write_image_to_ppm(image, "out.ppm");

}


void triangle(Vec2i v1, Vec2i v2, Vec2i v3, Image &image) {
    line(v1, v2, Color::white(), image);
    line(v2, v3, Color::white(),  image);
    line(v1, v3, Color::white(), image);
};

void chapter2() {
// Lesson 1: https://github.com/ssloy/tinyrenderer/tree/f6fecb7ad493264ecd15e230411bfb1cca539a12
const int width  = 200;
const int height = 200;
  Model model = parse_model("./obj/african_head/african_head.obj");
  // Model model = parse_model("./obj/tri.obj");
  Image image(width, height, Color::darkgray());
  cout << "model has |" << model.verts.size() << "| vertices\n";
  cout << "model has |" << model.faces.size() << "| faces\n";
  for(int f = 0; f < model.faces.size(); ++f) {
    vector<int> face = model.faces[f];
    cout << "have face: " << face << " {";
    for(int j = 0; j < 3; ++j) {
      cout << model.verts[face[j]] << " ";
    }
    cout << "}\n";
    Vec2i screen_space_face[3];
    for(int i = 0; i < 3; ++i) {
      Vec3f v = model.verts[face[i]];
      screen_space_face[i] = Vec2i((v.x + 1.) * width/2., (v.y + 1)*height/2.);
    }
    triangle(screen_space_face[0], screen_space_face[1], screen_space_face[2], image);

    // for(int j = 0; j < 3; ++j) {
    //   const Vec3f v0 = model.verts[face[j]];
    //   const Vec3f v1 = model.verts[face[(j+1)%3]];
    //   int x0 = (v0.x+1.)*width/2.;
    //   int y0 = (v0.y+1.)*height/2.;
    //   int x1 = (v1.x+1.)*width/2.;
    //   int y1 = (v1.y+1.)*height/2.;
    //   line(Vec2i(x0, y0), Vec2i(x1, y1), Color::white(), image);
    // }
    cout << "\n";
  }

  write_image_to_ppm(image, "out.ppm");
}

int main(){
  // chapter1();
  chapter2();
  return 0;
}
