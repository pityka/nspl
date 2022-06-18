// This code is derived from https://github.com/gfxfundamentals/webgl-fundamentals
//
// The original javascript code comes with this license:
//  Copyright 2021 GFXFundamentals.
//  All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  * Redistributions of source code must retain the above copyright
//  notice, this list of conditions and the following disclaimer.
//  * Redistributions in binary form must reproduce the above
//  copyright notice, this list of conditions and the following disclaimer
//  in the documentation and/or other materials provided with the
//  distribution.
//  * Neither the name of GFXFundamentals. nor the names of his
//  contributors may be used to endorse or promote products derived from
//  this software without specific prior written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
//  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
package org.nspl

import scala.{math => Math}

private[nspl] object Math3D {

  def degToRad(d: Double) =
    (d * math.Pi / 180).toFloat

  case class Mat4(ar: Array[Float]) {
    override def toString =
      s"${ar(0)} ${ar(1)} ${ar(2)} ${ar(3)}\n${ar(4)} ${ar(5)} ${ar(6)} ${ar(7)}\n${ar(
        8
      )} ${ar(9)} ${ar(10)} ${ar(11)}\n${ar(12)} ${ar(13)} ${ar(14)} ${ar(15)}"
    def apply(i: Int) = ar(i)
  }
  object Mat4 {
    def empty = Mat4(Array.ofDim[Float](16))
    def apply(f: Float*): Mat4 = Mat4(f.toArray)
  }
  case class Vec3(ar: Array[Float]) {
    override def toString = s"Vec3(${ar.mkString(",")})"
    def apply(i: Int) = ar(i)
    def toPoint = Point(ar(0), ar(1))
    def withZ(z: Float) = Vec3(this(0), this(1), z)
    def withX(x: Float) = Vec3(x, this(1), this(2))
    def withY(y: Float) = Vec3(this(0), y, this(2))
    def *(f: Float) = Vec3(this(0) * f, this(1) * f, this(2) * f)
    def +(f: Vec3) = Vec3(this(0) + f(0), this(1) + f(1), this(2) + f(2))
  }
  object Vec3 {
    val empty = Vec3(Array(0f, 0f, 0f))
    def apply(a: Float, b: Float, c: Float): Vec3 = Vec3(Array(a, b, c))
  }
  case class Vec4(ar: Array[Float]) {
    override def toString = s"Vec4(${ar.mkString(",")})"
    def apply(i: Int) = ar(i)
  }
  object Vec4 {
    val empty = Vec3(Array(0f, 0f, 0f, 1f))
    def apply(a: Float, b: Float, c: Float, d: Float): Vec4 =
      Vec4(Array(a, b, c, d))
  }

  def normalize(v: Vec3) = {
    val length = math.sqrt(v(0) * v(0) + v(1) * v(1) + v(2) * v(2)).toFloat
    if (length > 0.00001) {
      Vec3(v(0) / length, v(1) / length, v(2) / length)
    } else {
      Vec3.empty
    }
  }

  def subtractVectors(a: Vec3, b: Vec3) =
    Vec3(a(0) - b(0), a(1) - b(1), a(2) - b(2))

  def cross(a: Vec3, b: Vec3) =
    Vec3(
      a(1) * b(2) - a(2) * b(1),
      a(2) * b(0) - a(0) * b(2),
      a(0) * b(1) - a(1) * b(0)
    )

  def lookAt(cameraPosition: Vec3, target: Vec3, up: Vec3) = {
    var zAxis = normalize(subtractVectors(cameraPosition, target));
    var xAxis = normalize(cross(up, zAxis));
    var yAxis = normalize(cross(zAxis, xAxis));

    Mat4(
      xAxis(0),
      xAxis(1),
      xAxis(2),
      0,
      yAxis(0),
      yAxis(1),
      yAxis(2),
      0,
      zAxis(0),
      zAxis(1),
      zAxis(2),
      0,
      cameraPosition(0),
      cameraPosition(1),
      cameraPosition(2),
      1
    )
  }
  def perspective(
      fieldOfViewInRadians: Float,
      aspect: Float,
      near: Float,
      far: Float
  ) = {
    val f = Math.tan(Math.Pi * 0.5 - 0.5 * fieldOfViewInRadians).toFloat
    val rangeInv = 1.0f / (near - far);

    Mat4(
      f / aspect,
      0,
      0,
      0,
      0,
      f,
      0,
      0,
      0,
      0,
      (near + far) * rangeInv,
      -1,
      0,
      0,
      near * far * rangeInv * 2,
      0
    )
  }
  //   projection: function(width, height, depth) {
//     // Note: This matrix flips the Y axis so 0 is at the top.
//     return (
//        2 / width, 0, 0, 0,
//        0, -2 / height, 0, 0,
//        0, 0, 2 / depth, 0,
//       -1, 1, 0, 1,
//     );
//   },

  def multiply(a: Mat4, b: Mat4) = {
    val a00 = a(0 * 4 + 0);
    val a01 = a(0 * 4 + 1);
    val a02 = a(0 * 4 + 2);
    val a03 = a(0 * 4 + 3);
    val a10 = a(1 * 4 + 0);
    val a11 = a(1 * 4 + 1);
    val a12 = a(1 * 4 + 2);
    val a13 = a(1 * 4 + 3);
    val a20 = a(2 * 4 + 0);
    val a21 = a(2 * 4 + 1);
    val a22 = a(2 * 4 + 2);
    val a23 = a(2 * 4 + 3);
    val a30 = a(3 * 4 + 0);
    val a31 = a(3 * 4 + 1);
    val a32 = a(3 * 4 + 2);
    val a33 = a(3 * 4 + 3);
    val b00 = b(0 * 4 + 0);
    val b01 = b(0 * 4 + 1);
    val b02 = b(0 * 4 + 2);
    val b03 = b(0 * 4 + 3);
    val b10 = b(1 * 4 + 0);
    val b11 = b(1 * 4 + 1);
    val b12 = b(1 * 4 + 2);
    val b13 = b(1 * 4 + 3);
    val b20 = b(2 * 4 + 0);
    val b21 = b(2 * 4 + 1);
    val b22 = b(2 * 4 + 2);
    val b23 = b(2 * 4 + 3);
    val b30 = b(3 * 4 + 0);
    val b31 = b(3 * 4 + 1);
    val b32 = b(3 * 4 + 2);
    val b33 = b(3 * 4 + 3);
    Mat4(
      b00 * a00 + b01 * a10 + b02 * a20 + b03 * a30,
      b00 * a01 + b01 * a11 + b02 * a21 + b03 * a31,
      b00 * a02 + b01 * a12 + b02 * a22 + b03 * a32,
      b00 * a03 + b01 * a13 + b02 * a23 + b03 * a33,
      b10 * a00 + b11 * a10 + b12 * a20 + b13 * a30,
      b10 * a01 + b11 * a11 + b12 * a21 + b13 * a31,
      b10 * a02 + b11 * a12 + b12 * a22 + b13 * a32,
      b10 * a03 + b11 * a13 + b12 * a23 + b13 * a33,
      b20 * a00 + b21 * a10 + b22 * a20 + b23 * a30,
      b20 * a01 + b21 * a11 + b22 * a21 + b23 * a31,
      b20 * a02 + b21 * a12 + b22 * a22 + b23 * a32,
      b20 * a03 + b21 * a13 + b22 * a23 + b23 * a33,
      b30 * a00 + b31 * a10 + b32 * a20 + b33 * a30,
      b30 * a01 + b31 * a11 + b32 * a21 + b33 * a31,
      b30 * a02 + b31 * a12 + b32 * a22 + b33 * a32,
      b30 * a03 + b31 * a13 + b32 * a23 + b33 * a33
    );
  }

  def translation(tx: Float, ty: Float, tz: Float) = {
    Mat4(
      1,
      0,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      0,
      1,
      0,
      tx,
      ty,
      tz,
      1
    )
  }

//   xRotation: function(angleInRadians) {
//     var c = Math.cos(angleInRadians);
//     var s = Math.sin(angleInRadians);

//     return (
//       1, 0, 0, 0,
//       0, c, s, 0,
//       0, -s, c, 0,
//       0, 0, 0, 1,
//     );
//   },

//   yRotation: function(angleInRadians) {
//     var c = Math.cos(angleInRadians);
//     var s = Math.sin(angleInRadians);

//     return (
//       c, 0, -s, 0,
//       0, 1, 0, 0,
//       s, 0, c, 0,
//       0, 0, 0, 1,
//     );
//   },

//   zRotation: function(angleInRadians) {
//     var c = Math.cos(angleInRadians);
//     var s = Math.sin(angleInRadians);

//     return (
//        c, s, 0, 0,
//       -s, c, 0, 0,
//        0, 0, 1, 0,
//        0, 0, 0, 1,
//     );
//   },

//   scaling: function(sx, sy, sz) {
//     return (
//       sx, 0,  0,  0,
//       0, sy,  0,  0,
//       0,  0, sz,  0,
//       0,  0,  0,  1,
//     );
//   },

  def translate(m: Mat4, tx: Float, ty: Float, tz: Float) =
    multiply(m, translation(tx, ty, tz))

//   xRotate: function(m, angleInRadians) {
//     return m4.multiply(m, m4.xRotation(angleInRadians));
//   },

//   yRotate: function(m, angleInRadians) {
//     return m4.multiply(m, m4.yRotation(angleInRadians));
//   },

//   zRotate: function(m, angleInRadians) {
//     return m4.multiply(m, m4.zRotation(angleInRadians));
//   },

//   scale: function(m, sx, sy, sz) {
//     return m4.multiply(m, m4.scaling(sx, sy, sz));
//   },

  def inverse(m: Mat4) = {
    val m00 = m(0 * 4 + 0);
    val m01 = m(0 * 4 + 1);
    val m02 = m(0 * 4 + 2);
    val m03 = m(0 * 4 + 3);
    val m10 = m(1 * 4 + 0);
    val m11 = m(1 * 4 + 1);
    val m12 = m(1 * 4 + 2);
    val m13 = m(1 * 4 + 3);
    val m20 = m(2 * 4 + 0);
    val m21 = m(2 * 4 + 1);
    val m22 = m(2 * 4 + 2);
    val m23 = m(2 * 4 + 3);
    val m30 = m(3 * 4 + 0);
    val m31 = m(3 * 4 + 1);
    val m32 = m(3 * 4 + 2);
    val m33 = m(3 * 4 + 3);
    val tmp_0 = m22 * m33;
    val tmp_1 = m32 * m23;
    val tmp_2 = m12 * m33;
    val tmp_3 = m32 * m13;
    val tmp_4 = m12 * m23;
    val tmp_5 = m22 * m13;
    val tmp_6 = m02 * m33;
    val tmp_7 = m32 * m03;
    val tmp_8 = m02 * m23;
    val tmp_9 = m22 * m03;
    val tmp_10 = m02 * m13;
    val tmp_11 = m12 * m03;
    val tmp_12 = m20 * m31;
    val tmp_13 = m30 * m21;
    val tmp_14 = m10 * m31;
    val tmp_15 = m30 * m11;
    val tmp_16 = m10 * m21;
    val tmp_17 = m20 * m11;
    val tmp_18 = m00 * m31;
    val tmp_19 = m30 * m01;
    val tmp_20 = m00 * m21;
    val tmp_21 = m20 * m01;
    val tmp_22 = m00 * m11;
    val tmp_23 = m10 * m01;

    val t0 = (tmp_0 * m11 + tmp_3 * m21 + tmp_4 * m31) -
      (tmp_1 * m11 + tmp_2 * m21 + tmp_5 * m31);
    val t1 = (tmp_1 * m01 + tmp_6 * m21 + tmp_9 * m31) -
      (tmp_0 * m01 + tmp_7 * m21 + tmp_8 * m31);
    val t2 = (tmp_2 * m01 + tmp_7 * m11 + tmp_10 * m31) -
      (tmp_3 * m01 + tmp_6 * m11 + tmp_11 * m31);
    val t3 = (tmp_5 * m01 + tmp_8 * m11 + tmp_11 * m21) -
      (tmp_4 * m01 + tmp_9 * m11 + tmp_10 * m21);

    val d = 1.0f / (m00 * t0 + m10 * t1 + m20 * t2 + m30 * t3);

    Mat4(
      d * t0,
      d * t1,
      d * t2,
      d * t3,
      d * ((tmp_1 * m10 + tmp_2 * m20 + tmp_5 * m30) -
        (tmp_0 * m10 + tmp_3 * m20 + tmp_4 * m30)),
      d * ((tmp_0 * m00 + tmp_7 * m20 + tmp_8 * m30) -
        (tmp_1 * m00 + tmp_6 * m20 + tmp_9 * m30)),
      d * ((tmp_3 * m00 + tmp_6 * m10 + tmp_11 * m30) -
        (tmp_2 * m00 + tmp_7 * m10 + tmp_10 * m30)),
      d * ((tmp_4 * m00 + tmp_9 * m10 + tmp_10 * m20) -
        (tmp_5 * m00 + tmp_8 * m10 + tmp_11 * m20)),
      d * ((tmp_12 * m13 + tmp_15 * m23 + tmp_16 * m33) -
        (tmp_13 * m13 + tmp_14 * m23 + tmp_17 * m33)),
      d * ((tmp_13 * m03 + tmp_18 * m23 + tmp_21 * m33) -
        (tmp_12 * m03 + tmp_19 * m23 + tmp_20 * m33)),
      d * ((tmp_14 * m03 + tmp_19 * m13 + tmp_22 * m33) -
        (tmp_15 * m03 + tmp_18 * m13 + tmp_23 * m33)),
      d * ((tmp_17 * m03 + tmp_20 * m13 + tmp_23 * m23) -
        (tmp_16 * m03 + tmp_21 * m13 + tmp_22 * m23)),
      d * ((tmp_14 * m22 + tmp_17 * m32 + tmp_13 * m12) -
        (tmp_16 * m32 + tmp_12 * m12 + tmp_15 * m22)),
      d * ((tmp_20 * m32 + tmp_12 * m02 + tmp_19 * m22) -
        (tmp_18 * m22 + tmp_21 * m32 + tmp_13 * m02)),
      d * ((tmp_18 * m12 + tmp_23 * m32 + tmp_15 * m02) -
        (tmp_22 * m32 + tmp_14 * m02 + tmp_19 * m12)),
      d * ((tmp_22 * m22 + tmp_16 * m02 + tmp_21 * m12) -
        (tmp_20 * m12 + tmp_23 * m22 + tmp_17 * m02))
    )
  }

  def vectorMultiply(v: Vec4, m: Mat4) = {
    val dst = Array.ofDim[Float](4)
    var i = 0
    var j = 0
    while (i < 4) {
      while (j < 4) {
        dst(i) += v(j) * m(j * 4 + i)
        j += 1
      }
      j = 0
      i += 1
    }

    Vec4(dst)
  }

  def perspectiveDivide(v: Vec4): Vec3 =
    Vec3(v(0) / v(3), v(1) / v(3), v(2) / v(3))

}
