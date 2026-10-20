package org.nspl

import org.nspl.data.KDE

class KdeContribSpec extends munit.FunSuite {

  test("density2dAnisotropic matches isotropic density2d when bandwidths are equal") {
    val data = Vector((0.0, 0.0), (1.0, 1.0), (2.0, 0.5), (-1.0, 1.5))
    val point = (0.5, 0.5)
    val isotropic = KDE.density2d(data, point, 0.7)
    val anisotropic = KDE.density2dAnisotropic(data, point, 0.7, 0.7)
    assertEqualsDouble(anisotropic, isotropic, 1e-12)
  }

  test("density2dAnisotropic is strictly positive") {
    val data = Vector((0.0, 0.0), (3.0, 4.0))
    assert(KDE.density2dAnisotropic(data, (1.0, 1.0), 0.5, 2.0) > 0d)
  }

  test("silvermanBandwidth is positive and finite for varied data") {
    val bandwidth = silvermanBandwidth(Vector(1.0, 2.0, 3.0, 4.0, 5.0, 100.0))
    assert(bandwidth > 0d && bandwidth.isFinite)
  }

  test("silvermanBandwidth falls back to one for constant data") {
    assertEqualsDouble(silvermanBandwidth(Vector(2.0, 2.0, 2.0, 2.0)), 1d, 1e-12)
  }
}
