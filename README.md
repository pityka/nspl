[User documentation](https://pityka.github.io/nspl/)

[![](https://img.shields.io/maven-central/v/io.github.pityka/nspl-core_2.12.svg)](https://search.maven.org/search?q=g:io.github.pityka%20nspl)



# 2d Plotting Library for Scala (including scala-js)

Creates scatter, line, contour, raster, barplots and boxplots, histograms, density plots. Composite figure of multiple plots. 

Renders to
* java Graphics2D
* PDF/SVG/EPS (via [VectorGraphics2D](https://github.com/eseifert/vectorgraphics2d))
* SVG (via [ScalaTags](http://www.lihaoyi.com/scalatags/), both in browser or on jvm)
* Html5 Canvas ([scala-js](http://www.scala-js.org))


## Usage

See the documentation website at [https://pityka.github.io/nspl/](https://pityka.github.io/nspl/) .

Maven coordinates: 

* JVM AWT backend: `libraryDependencies += "io.github.pityka" %% "nspl-awt" % "0.0.???"`
* JVM SVG via scalatags: `libraryDependencies += "io.github.pityka" %% "nspl-scalatags-jvm" % "0.0.???"`
* scala-js svg via scalatags: `libraryDependencies += "io.github.pityka" %% "nspl-scalatags-js" % "0.0.???"` 
* scala-js canvas via scalatags: `libraryDependencies += "io.github.pityka" %% "nspl-canvas-js" % "0.0.???"` 

## Version policy

nspl uses 'early-semver' versioning enforced with https://github.com/scalacenter/sbt-version-policy. 

## How to build the website
The website is built with [hugo](https://gohugo.io/) and the [hugo-book](https://github.com/alex-shpak/hugo-book) theme.

The theme is a git submodule. It must be initialized.

```sh
git submodule update --init 
```

Create and serve the site with:

```sh
sbt docs/mdoc docs/unidoc && cd website && hugo
```