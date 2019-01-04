# incomplete attempt at drawing a multi-segment line where each segment can have a different width and colour

# Draws a multi-line segment, each segment having a different colour and width
# widths - Width at each point (not width of segment)
# colours - colour of each segment
DrawVarLine <- function(x, y, widths, col = "black", add = FALSE, xlim, ylim, fastJoins = FALSE, roundCorners = TRUE, ...) {
  if (length(col) == 1)
    col <- rep(col, length(x))
  pts <- StrokeVarLine(x, y, widths, fastJoins = fastJoins, roundCorners = roundCorners)
  allPts <- c(pts$left, pts$right)

  # Optionally create the (empty) plot
  if (!add) {
    if (missing(xlim))
      xlim <- range(Re(allPts))
    if (missing(ylim))
      ylim <- range(Im(allPts))
    plot(NULL, xlim = xlim, ylim = ylim, ...)
  }

  # The points travel up the left hand side and back down the right hand side
  leftPts <- pts$left
  rightPts <- pts$right
  # points(leftPts, col = "red", pch = 16)
  # points(rightPts, col = "blue", pch = 16)
  for (i in 1:(length(x)-1)) {
    # THIS IS NOW WRONG
    polygon(c(leftPts[i:(i + 1)], rightPts[(i + 1):i]), col = col[i], border = NA)
  }
}

# Strokes a multi-segment line, each segment having a different width.
# @param pts Set of complex points which represent the segment end points
# @param segments Complex vectors equivalent to diff(pts)
# @param widths Width at each point (not width of segment)
#
# @return Set of complex points which are the points defining the border
# of the variable width poly-line.
.StrokeVarLine <- function(pts, segments = NULL, widths, fastJoins = FALSE, roundCorners = FALSE) {
  np <- length(pts)
  if (length(widths) == 1)
    widths <- rep(widths, np)

  # Input data checks
  if (is.null(segments))
    segments <- diff(pts)
  if (np != length(widths))
    stop(sprintf("pts (%d) and widths (%d) must have the same length", length(pts), length(widths)))
  if (np != length(segments) + 1)
    stop("segments (%d) must have length 1 less than pts (%d)", length(segments), length(pts))
  naOrNulls <- is.na(widths) | is.null(widths)
  if (sum(naOrNulls))
    stop(sprintf("widths must not contain NA or NULL values (position%s %s)",
                 ifelse(sum(naOrNulls) == 1, "", "s"), paste(which(is.na(widths)), collapse = ", ")))

  ###
  # Calculates the mean of 2 or more angles
  .meanAngles <- function(angles) {
    Arg(sum(complex(modulus = 1, argument = angles)))
  }

  .pointsOnCurve <- function(startAngle, endAngle) {
    # The curve has to go the shortest way between the 2 angles
    while (endAngle - startAngle > pi)
      endAngle <- endAngle - 2 * pi
    while (endAngle - startAngle <= -pi)
      endAngle <- endAngle + 2 * pi

    # Generate points on the curve. Pick a set of equally spaced angles,
    # then the points are just the segment end-point offset by the the segment width
    # and each of the angles
    np <- abs(round(endAngle - startAngle) / (pi / 6))
    p <- complex(np)
    angles <- seq(startAngle, endAngle, length.out = np)
    for (ai in seq_along(angles)) {
      p[ai] <- pts[i] + complex(modulus = widths[i] / 2, argument = angles[ai])
    }
    p
  }

  ###

  # Allocate output point vectors
  extra <- ifelse(roundCorners, np * 2, 0)
  leftPts <- numeric(np + extra)
  rightPts <- numeric(np + extra)
  # Indices to keep track of current positions in output vectors
  leftIdx <- 0
  rightIdx <- 0

  # Duplicate last segment so we can access last + 1 == last
  dSegments <- c(segments, segments[length(segments)])
  prevAngle <- Arg(segments[1])

  # For each point...
  for(i in seq_along(pts)) {

    # Increment output point indices
    leftIdx <- leftIdx + 1
    rightIdx <- rightIdx + 1

    # Get mean angle of this and previous segments
    thisAngle <- Arg(dSegments[i])
    meanAngle <- .meanAngles(c(thisAngle, prevAngle))

    # Calculate points on mean cross-piece
    leftMCP <- pts[i] + complex(modulus = widths[i] / 2, argument = meanAngle + pi / 2)
    rightMCP <- pts[i] + complex(modulus = widths[i] / 2, argument = meanAngle + 3 * pi / 2)

    if (fastJoins) {
      leftPts[leftIdx] <- leftMCP
      rightPts[rightIdx] <- rightMCP
    } else {
      # Calculate perpendicular cross-piece
      leftPerpOffset <- complex(modulus = widths[i] / 2, argument = thisAngle + pi / 2)
      rightPerpOffset <- complex(modulus = widths[i] / 2, argument = thisAngle + 3 * pi / 2)

      if (i == 1) {
        # First start segment is simple perpendicular cross piece
        leftPts[leftIdx] <- pts[i] + leftPerpOffset
        rightPts[rightIdx] <- pts[i] + rightPerpOffset
      } else {
        # Calculate intersection of previous segment borders and the mean cross-piece
        leftPts[leftIdx] <- LinesIntersection(leftPts[leftIdx-1], leftPts[leftIdx], pts[i], leftMCP)
        rightPts[rightIdx] <- LinesIntersection(rightPts[rightIdx-1], rightPts[rightIdx], pts[i], rightMCP)
        if (is.na(leftPts[leftIdx]) || is.infinite(leftPts[leftIdx]) || is.na(rightPts[rightIdx]) || is.infinite(rightPts[rightIdx])) {
          # Fallback to fast method
          leftPts[leftIdx] <- leftMCP
          rightPts[rightIdx] <- rightMCP
        }

        # Maybe round outside corners
        diff <- thisAngle - prevAngle
        if (roundCorners && i < np) {
          # Determine which side is the outside
          roundLeft <- diff > pi || (diff < 0 && diff > -pi)
          # Calculate start and end angles for curve
          if (roundLeft) {
            startAngle <- prevAngle + pi / 2
            endAngle <- thisAngle - 3 * pi / 2
            cp <- .pointsOnCurve(startAngle, endAngle)
            ncp <- length(cp) - 1
            if (ncp > 0) {
              leftPts[leftIdx:(leftIdx+ncp)] <- cp
              leftIdx <- leftIdx + ncp
            }
          } else {
            startAngle <- prevAngle + 3 * pi / 2
            endAngle <- thisAngle + 3 * pi / 2
            cp <- .pointsOnCurve(startAngle, endAngle)
            ncp <- length(cp) - 1
            if (ncp > 0) {
              rightPts[rightIdx:(rightIdx+ncp)] <- cp
              rightIdx <- rightIdx + ncp
            }
          }

          # Check for pathological inside corner
          # Solution is a hack
          if (i > 1) {
            # Don't allow line from midpoint to corner point to exceed length of either adjacent segment
            maxAllowable <- min(Mod(segments[i-1]), Mod(segments[i]))
            if (roundLeft) {
              dist <- Mod(pts[i] - rightPts[rightIdx])
              if (dist > maxAllowable) {
                rightPts[rightIdx] <- pts[i] + complex(modulus = maxAllowable, argument = meanAngle + 3 * pi / 2)
              }
            } else {
              dist <- Mod(pts[i] - leftPts[leftIdx])
              if (dist > maxAllowable) {
                leftPts[leftIdx] <- pts[i] + complex(modulus = maxAllowable, argument = meanAngle + pi / 2)
              }
            }
          }
        }
      }

      if (i < np) {
        # Calculate end of segment cross-piece, for use when we do the next segment
        leftPts[leftIdx+1] <- pts[i+1] + leftPerpOffset
        rightPts[rightIdx+1] <- pts[i+1] + rightPerpOffset
      }
    }


    # This angle becomes the previous angle for the next segment
    prevAngle <- thisAngle
  }

  list(left = leftPts[1:leftIdx], right = rightPts[1:rightIdx])
}

# Strokes a multi-line segment, each segment having a different width.
# @param pts Set of complex points which represent the segment end points
# @param segments Complex vectors equaivalent to diff(pts)
# @param widths Width at each point (not width of segment)
# @param fastJoins Uses a faster but less accurate method to calculate
#        segment end points. Lines may not have the correct width when
#        using this method.
#
# @return Set of complex points which are the points defining the border
# of the variable width poly-line. The first `n` points define the left-hand-side from start to finish,
# and the next `n` points define the right-hand-side from finish to start.
StrokeVarLine <- function(x, y, widths, fastJoins = FALSE, roundCorners = FALSE) {
  .StrokeVarLine(complex(real = x, imaginary = y), widths = widths, fastJoins = fastJoins, roundCorners = roundCorners)
}

# Calculates the point of intersection between the
# unbounded lines (p1, p2) and (p3, p4)
LinesIntersection <- function(p1, p2, p3, p4) {
  # Calculate the intersection point using determinants
  # https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
  x1 <- Re(p1)
  y1 <- Im(p1)
  x2 <- Re(p2)
  y2 <- Im(p2)
  x3 <- Re(p3)
  y3 <- Im(p3)
  x4 <- Re(p4)
  y4 <- Im(p4)
  x <- ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) /
    ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))
  y <- ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) /
    ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))
  complex(real = x, imaginary = y)
}

###############################################################################

r2d <- function(r) 360 * r / (2 *pi)
arr <- function(p1, p2, add = TRUE, ...) { if (!add) plot(NULL, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1); arrows(Re(p1), Im(p1), Re(p2), Im(p2), ...) }
arrAng <- function(rad, len = 1, ...) arr(complex(1), complex(modulus = len, argument = rad), ...)

.plotPoly <- function(x, y, fastJoins, roundCorners, add = FALSE, xlim, ylim) {
  pts <- StrokeVarLine(x, y, widths = .1, fastJoins = fastJoins, roundCorners = roundCorners)
  allPts <- c(pts$left, rev(pts$right))
  np <- length(pts$left)
  if (!add) {
    if (missing(xlim))
      xlim <- range(Re(allPts))
    if (missing(ylim))
      ylim <- range(Im(allPts))
    plot(NULL, xlim = xlim, ylim = ylim, asp = 1)
  }
  points(pts$left, pch = 16, col = "red")
  points(pts$right, pch = 16, col = "blue")
  polygon(allPts, col = "#ff000030", border = NA)
  lines(pts$left, col = "red")
  lines(pts$right, col = "blue")
  points(x, y, pch = 16, col = "green")
  lines(x, y, col = "#00ff00")
  if (!roundCorners)
    segments(Re(pts$left), Im(pts$left), Re(pts$right), Im(pts$right))

  p <- complex(real = x, imaginary = y)
  a1 <- Arg(p[2] - p[1])
  a2 <- Arg(p[3] - p[2])
  text(x[2] + .1, y[2] - .1, round(r2d(a2 - a1)), pos = 4, cex = 2)
}

Doco <- function() {

  x1 <- c(0, 1.8, 2.5)
  y1 <- c(0, .5, 2) - .1

  x2 <- c(0.2, 1.5, 0.2)
  y2 <- c(1.3, 1.6, 1.9)

  par(mfrow = c(1, 2))
  .plotPoly(x1, y1, TRUE, FALSE)
  .plotPoly(x2, y2, TRUE, FALSE, add = TRUE)

  .plotPoly(x1, y1, FALSE, TRUE)
  .plotPoly(x2, y2, FALSE, TRUE, add = TRUE)
}

AltVarLines <- function(x, y, widths, col, ...) {
  n <- length(x)
  if (length(widths) == 1)
    widths <- rep(widths, n)
  segments(x[1:(n-1)], y[1:(n-1)], x[2:n], y[2:n], lwd = widths, col = col)
}

tests <- function() {

  .clock <- function(angle) {
    x1 <- c(0, 0, cos(angle))
    y1 <- c(-1, 0, sin(angle))
    .plotPoly(x1, y1, FALSE, TRUE, add = F, xlim = c(-1, 1), ylim = c(-1, 1))
    #plot(NULL, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
    #AltVarLines(x1, y1, 20, "#ff000030")
  }
  JAnimateGIF(frameKeys = seq(0, 2 * pi, pi / 48), gifFileName = "test.gif", plotFn = .clock, width = 900, height = 900, units = "px")

  # Left corner needs rounding
  x1 <- c(-.2, 0, .2)
  y1 <- c(0, 1, 0)
  .plotPoly(x1, y1, F, TRUE, add = F)
  AltVarLines(x1, y1, 10, "#ff000030", asp = 1)

  # Right corner needs rounding
  x1 <- c(.2, 0, -.2)
  y1 <- c(0, 1, 0)
  .plotPoly(x1, y1, F, TRUE, add = F)

  # Wrong inner corner
  x1 <- c(.02, 0, -.02)
  y1 <- c(0, 1, 0)
  .plotPoly(x1, y1, F, TRUE, add = F)
  AltVarLines(x1, y1, 10, "#ff000030", asp = 1)

  x1 <- c(-.02, 0, .02)
  y1 <- c(0, 1, 0)
  .plotPoly(x1, y1, F, TRUE, add = F)
  AltVarLines(x1, y1, 10, "#ff000030", asp = 1)

  # More complex lines
  x1 <- c(0, .1, 0)
  y1 <- c(0, .1, -.01)
  .plotPoly(x1, y1, F, TRUE)

  x1 <- c(0, .1, 0, .5)
  y1 <- c(0, .1, .1, .15)
  #plot(x1, y1, col = "green", type = 'l', lwd = 2)
  .plotPoly(x1, y1, FALSE, TRUE, add = F)
  DrawVarLine(x1, y1, widths = .01)
}
