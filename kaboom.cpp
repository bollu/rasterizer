// https://github.com/ssloy/tinykaboom/wiki/KABOOM!-in-180-lines-of-code

#include <assert.h>
#include <cmath>
#include <algorithm>
#include <limits>
#include <iostream>
#include <fstream>
#include <vector>
using namespace std;
// #include "geometry.h"


template<typename T>
struct Vec3T { 
  T x, y, z;
  Vec3T() : x(0), y(0), z(0) {}
  Vec3T(T x, T y, T z): x(x), y(y), z(z) {}
  double norm() const {
      double lensq = x*x + y*y + z*z;
      double len = std::sqrt(lensq);
      return len;
  }
  Vec3T normalize() const {
      const double len = norm();
      return Vec3T(x/len, y/len, z/len);
  }

  float operator() (int i) {
      assert(i < 3);
      assert(i >= 0);
      if (i == 0) { return x; }
      if (i == 1) { return y; }
      else { return z; }
  }

};

template<typename T>
ostream &operator << (ostream &o, Vec3T<T> v) {
  return o << "v3(" << v.x << "  " << v.y << " " << v.z << ")";
}

template<typename T>
Vec3T<T> operator *(const Vec3T<T> v, const T f) {
    return Vec3T<T>(v.x*f, v.y*f, v.z*f);
}

template<typename T>
Vec3T<T> operator - (const Vec3T<T> v, const Vec3T<T> w) {
    return Vec3T<T>(v.x - w.x, v.y - w.y, v.z - w.z);
}


template<typename T>
float operator *(const Vec3T<T> v, const Vec3T<T> w) {
    return v.x*w.x +  v.y*w.y + v.z*w.z;
}


template<typename T>
Vec3T<T> operator +(const Vec3T<T> v, const Vec3T<T> w) {
    return Vec3T<T>(v.x + w.x, v.y + w.y, v.z + w.z);
}

using Vec3i = Vec3T<int>;
using Vec3f = Vec3T<float>;


const float sphere_radius   = 1.5;

float signed_distance(const Vec3f &p) {
    return p.norm() - sphere_radius;
}

bool sphere_trace(const Vec3f &orig, const Vec3f &dir, Vec3f &pos) {
    pos = orig;
    for (size_t i=0; i<128; i++) {
        float d = signed_distance(pos);
        if (d < 0) return true;
        pos = pos + dir*std::max(d*0.1f, .01f);
    }
    return false;
}


// find normal to SDF
// recall that normal to level set f(x) = 0 is grad f!
// compute grad by finite difference.
Vec3f distance_field_normal(const Vec3f &pos) {
    const float eps = 0.1;
    float d = signed_distance(pos);
    float nx = signed_distance(pos + Vec3f(eps, 0, 0)) - d;
    float ny = signed_distance(pos + Vec3f(0, eps, 0)) - d;
    float nz = signed_distance(pos + Vec3f(0, 0, eps)) - d;
    return Vec3f(nx, ny, nz).normalize();
}



int main() {
    const int   width    = 640;
    const int   height   = 480;
    const float fov      = M_PI/3.;
    std::vector<Vec3f> framebuffer(width*height);

#pragma omp parallel for
    for (size_t j = 0; j<height; j++) { // actual rendering loop
        for (size_t i = 0; i<width; i++) {
            float dir_x =  (i + 0.5) -  width/2.;
            float dir_y = -(j + 0.5) + height/2.;    // this flips the image at the same time
            float dir_z = -height/(2.*tan(fov/2.));
            Vec3f hit;

            if (sphere_trace(Vec3f(0, 0, 3), Vec3f(dir_x, dir_y, dir_z).normalize(), hit)) { // the camera is placed to (0,0,3) and it looks along the -z axis
                // | one light is placed to (10,10,10)
                Vec3f light_dir = (Vec3f(10, 10, 10) - hit).normalize(); 
                float light_intensity  = std::max(0.4f, light_dir*distance_field_normal(hit));
                framebuffer[i+j*width] = Vec3f(1, 1, 1)*light_intensity;
            } else {
                framebuffer[i+j*width] = Vec3f(0.2, 0.7, 0.8); // background color
            }
        }
    }

    std::ofstream ofs("./out.ppm", std::ios::binary); // save the framebuffer to file
    ofs << "P6\n" << width << " " << height << "\n255\n";
    for (size_t i = 0; i < height*width; ++i) {
        for (size_t j = 0; j<3; j++) {
            ofs << (char)(std::max(0, std::min(255, static_cast<int>(255*framebuffer[i](j)))));
        }
    }
    ofs.close();

    return 0;
}
