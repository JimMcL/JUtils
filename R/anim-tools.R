# Set of functions to aid in creating pleasing animations
#
# An animation is constructed by building a list of scenes.  A scene
# is a parameterised plotting function, together with set of
# transitions. A transition defines how the values of a parameter
# change over time during the scene.
#
# Transitions make use of bezier curves for flexible smooth
# transitions.  Transitions are largely inspired by CSS animations.


# See https://cubic-bezier.com/
library(bezier)

# ================================================================================
#### Private functions ####

# Interpolates a single segment of points, one for each frame
#
# @param from Initial paramter value
# @param to Final parameter value
# @param nPoints Number of points to derive, including the the start and end points.
# @param timing A timing function.
interpValues <- function(from, to, nPoints, timing) {
  # Start with timing values
  f <- timing(seq(0, 1, length.out = nPoints))
  # Interpolate
  from + (to - from) * f
}

interpTrans <- function(trans, nFrames) {
  # Convert times to frame numbers
  keyFrames <- round(trans$times * (nFrames - 1) + 1)

  # Initialise with from values
  vals <- rep.int(trans$from, nFrames)

  # Fill in transition
  change <- interpValues(trans$from, trans$to, keyFrames[2] - keyFrames[1] + 1, trans$timing)

  vals[seq(keyFrames[1], keyFrames[2], 1)] <- change

  # Extend last value
  if (keyFrames[2] != nFrames) {
    vals[seq(keyFrames[2], nFrames, 1)] <- rep.int(trans$to, nFrames - keyFrames[2] + 1)
  }

  vals
}

timingFunctionBezier <- function(p0, p1, p2, p3, x) {
  # Cubic bezier curve
  t <- seq(0, 1, length.out = 40)
  p <- matrix(c(0, 0,  p0, p1,  p2, p3,  1, 1), ncol = 2, byrow = TRUE)
  b <- bezier::bezier(t, p)
  # Linearly interpolate to get points equally spaced along the x-axis
  y <- stats::approx(b, xout = x)

  y$y
}

timingFunctionStep <- function(stepTime, x) {
  # Get value for each frame
  ifelse(stepTime > x, 0, 1)
}

# ================================================================================
#### Public functions ####

# Timing functions
# Invoked by name only (no additional parameters are required)

#' Predefined transition timings
#'
#' These functions are passed by name (i.e. without following parentheses) as the
#' \code{timing} parameter to the \code{\link{JTransition}} function.
#'
#' @param x x-value passed to the function automatically. See \code{Examples} below.
#'
#' @return A timing function.
#'
#' @seealso \code{\link{JTransition}}, \code{\link{JScene}}, \code{\link{JBezier}}
#'
#' @examples
#' # Vary width from 0.1 to 1, with faster change in the middle of the scene
#' width = JTransition(0.1, 1, JEaseInOut)
#'
#' @export
JEase <- function(x) timingFunctionBezier(0.25, 0.1, 0.25, 1, x)
#' @rdname JEase
#' @export
JLinear <- function(x) timingFunctionBezier(0, 0, 1, 1, x)
#' @rdname JEase
#' @export
JEaseIn <- function(x) timingFunctionBezier(0.42, 0, 1, 1, x)
#' @rdname JEase
#' @export
JEaseOut <- function(x) timingFunctionBezier(0, 0, 0.58, 1, x)
#' @rdname JEase
#' @export
JEaseInOut <- function(x) timingFunctionBezier(0.42, 0, 0.58, 1, x)
#' @rdname JEase
#' @export
JBounce <- function(x) timingFunctionBezier(0.175, 0.885, 0.32, 1.275, x)

# Invoked by function call (additional parameters required)

#' Parameterised transition timings
#'
#' These functions allow you to create custom transition timing functions. See
#' Examples below for usage.
#'
#' @param p0,p1,p2,p3 Define the two control points (P1 & P2) of a cubic bezier
#'   curve with end points at (0, 0) and (1, 1).
#' @param time Time at which to step from initial value to final value. Time is
#'   expressed as a fraction of the scene duration. The scene starts at time 0
#'   and ends at time 1.
#' @return A timing function.
#'
#' @seealso \code{\link{JTransition}}, \code{\link{JEase}}, \code{\link{JScene}}
#'
#' @examples
#' # Replicate the \link{JEaseOut} timing function
#' width = JTransition(.1, 1, timing = JBezier(0, 0, 0.58, 1))
#'
#' @export
JBezier <- function(p0, p1, p2, p3) function(x) timingFunctionBezier(p0, p1, p2, p3, x)
#' @rdname JBezier
#' @export
JStep <- function(time) function(x) timingFunctionStep(time, x)

#' Construct a JTransition
#'
#' A JTransition defines how a single parameter values changes throughout a
#' scene. The parameter changes from \code{from} in the first frame (time
#' \code{0}), through to \code{to} in the final frame (time \code{1}). The way
#' it changes is defined by the parameters \code{timing} and \code{times}.
#'
#' It is possible to create custom timing functions. A timing function is a
#' function that accepts one argument, a numeric vector \code{x}, and returns a
#' mapping from \code{x} to some other value. For example, a simple linear
#' timing function could be implemented as \code{function(x) x}.
#'
#' @param from Initial parameter value.
#' @param to Final parameter value.
#' @param timing Animation timing function, such as \code{\link{JEaseIn}}, \code{\link{JBounce}}.
#' @param times Time period (start, stop) over which the transition occurs, as a
#'   proportion of the scene time. A time of 0 indicates the first frame in the
#'   scene, while 1 is the last frame. If the transition does not start at the
#'   beginning of the frame, for all earlier frames the parameter will have
#'   values `from`. Similarly, the parameter will have value `to` for any frames
#'   after the last transition frame.
#'
#' @return List used to define how a single parameter changes within a scene.
#'
#' @seealso \code{\link{JScene}}, \code{\link{JEase}}, \code{\link{JBezier}}
#'
#' @export
JTransition <- function(from, to, timing = JEase, times = c(0, 1)) {
  tr <- list(from = from, to = to, timing = timing, times = times)
  class(tr) <- c("JTransition", class(tr))
  tr
}

#' Construct a JScene
#'
#' A JScene is a portion of a complex animation. It consists of some metadata
#' (duration, frame rate), a set of animation transitions and a plotting
#' function.
#'
#' @param duration Scene duration in seconds.
#' @param fps Frame rate (frames per second).
#' @param startAfter Timing of the start of this frame. Number of seconds after
#'   the end of the previous scene that this scene starts.
#' @param ... Ordered set of transitions. Transitions may be named for
#'   documentation purposes, but names are not required.
#' @param plotFn The parameterised plotting function. Called with one positional
#'   argument for each transition, and an optional final boolean argument,
#'   \code{add}, which is \code{TRUE} if the function should add to an existing
#'   plot rather than create a new plot. The \code{add} is only used for
#'   overlapping scenes.
#'
#' @return A list, known as a \code{JScene}, that can be included in a list of
#'   scenes then used to create an animation.
#'
#' @seealso \code{\link{JAnimateScenes}}, \code{\link{JTransition}}, \code{\link{JPlotScenes}}
#'
#' @examples
#' # Construct a single-scene animation
#' scenes <- list(JScene(1, # Duration
#'                       20, # Frame rate
#'
#'                       # Parameters to be passed to plotFn
#'                       pt1 = JTransition(1, 0),
#'                       pt2 = JTransition(0, 1),
#'
#'                       # Plotting function with parameters matching those defined above
#'                       plotFn = function(pt1, pt2) {
#'                         plot(c(pt1, 1 - pt2), c(pt2, pt1), type = "b", xlim = c(0, 1), ylim = c(0, 1))
#'                       }))
#' # Plot 20 frames
#' for (i in 1:20) {
#'   JPlotScenes(scenes)(i)
#'   # Crude way to animate in real time
#'   dev.flush()
#'   Sys.sleep(0.05)
#' }
#'
#' @export
JScene <- function(duration, fps, startAfter = 0, ..., plotFn) {
  nFrames <- floor(duration * fps)
  transitions <- list(...)
  # Play silly buggers to handle variable args with optional startAfter.
  # If startAfter isn't specified, the first transition is interpreted as startAfter
  if (inherits(startAfter, "JTransition")) {
    transitions <- append(transitions, list(startAfter), after = 0)
    startAfter <- 0
  }
  badTransitions <- sapply(transitions, function(t) !inherits(t, "JTransition"))
  if (any(badTransitions)) {
    stop(sprintf("Invalid animation argument, use JTransition to define arguments"))
  }
  args <- lapply(transitions, interpTrans, nFrames)
  # Convert to data frame
  args <- do.call(cbind, args)
  # Does the function accept an "add" parameter?
  hasAddArg <- "add" %in% methods::formalArgs(match.fun(plotFn))

  # Return a function that draws 1 frame
  list(nFrames = nFrames,
       offset = round(startAfter * fps),
       fps = fps, # Just so it can be checked for consistency with other scenes
       fun = function(frame, add) {
         argList <- as.list(args[frame, ])
         if (hasAddArg) {
           argList <- c(argList, add = add)
         } else if (add) {
           stop("When scenes overlap, plotFn must accept a final \"add\" parameter. See ?JScene")
         }
         do.call(plotFn, argList)
       })
}

#' Construct a plotting function from a list of scenes.
#'
#' \code{JPlotScenes} is not usually called directly, rather it is invoked
#' internally from inside \code{\link{JAnimateScenes}}. Combines a list of
#' scenes, and returns a function that plots a single frame from the appropriate
#' scene. Can be useful for debugging an animation, because it can be used to
#' plot a single frame.
#'
#' @param scenes A list of scene objects, each one created by calling the
#'   \code{\link{JScene}} constructor.
#'
#' @return A function that accepts a single argument, a frame number, and plots
#'   the appropriate frame from the animation. The function is intended to be
#'   used as the \code{plotFn} argument to \code{\link{JAnimateGIF}}.
#'
#' @seealso \code{\link{JAnimateScenes}}
#'
#' @examples
#' # Construct an animation
#' scenes <- list(JScene(1, 20,
#'                pt2 = JTransition(0, 1),
#'                plotFn = function(pt2) {
#'                  plot(c(0, pt2), c(0, pt2), type = "b", xlim = c(0, 1))
#'                }))
#' # Plot frame 10 to see what it looks like
#' JPlotScenes(scenes)(10)
#'
#' @export
JPlotScenes <- function(scenes) {

  function(frame) {
    # Which scene(s) is this frame part of?
    startFrame <- 1
    sceneCount <- 0
    for (s in scenes) {
      # Allow for shifted start
      startFrame <- startFrame + s$offset
      endFrame <- startFrame + s$nFrames - 1
      if (frame >= startFrame && frame <= endFrame) {
        # Plot this frame
        s$fun(frame - startFrame + 1, add = sceneCount > 0)
        sceneCount <- sceneCount + 1
      }

      if (frame < startFrame) {
        break
      }

      startFrame <- endFrame + 1
    }

    if (sceneCount == 0)
      stop(sprintf("Error: couldn't find scene for frame %d", frame))
  }
}

#' Returns the total number of frames in a scene list.
#'
#' @param scenes A list of scene objects, each one created by calling the
#'   \code{\link{JScene}} constructor.
#'
#' @return Number of frames in the animation specified by \code{scenes}.
#'
#' @export
JScenesnFrames <- function(scenes) {
  sum(sapply(scenes, function(s) s$nFrames + s$offset))
}

#' Animate a list of \code{\link{JScene}}s.
#'
#' Animates a \code{list} of \code{\link{JScene}} objects by plotting each frame
#' and then combining them into a single animated GIF.
#'
#' See \code{\link{JAnimateGIF}} for general information about creating
#' animations from R.
#'
#' @param videoFileName Name of the GIF file to be created.
#' @param scenes A list of scene objects, each one created by calling the
#'   \code{\link{JScene}} constructor.
#' @param ... Additional parameters are passed on to \code{\link{JAnimateGIF}}.
#'
#' @return The list of arguments passed to the \code{\link{JAnimateGIF}}
#'   function (invisibly).
#'
#' @seealso \code{\link{JAnimateGIF}}, \code{\link{JScene}}, \code{\link{JTransition}}, \code{\link{JEase}}
#'
#' @examples
#' \dontrun{
#' scenes <- list(JScene(1, 20,
#'                       pt1 = JTransition(1, 0, JEaseInOut),
#'                       pt2 = JTransition(0, 1),
#'                       plotFn = function(pt1, pt2) {
#'                         plot(c(pt1, pt2), c(1, pt2), type = "b", xlim = c(0, 1), ylim = c(0, 1))
#'                       }),
#'
#'                JScene(1, 20,
#'                       pt1 = JTransition(0, 1, JEaseInOut),
#'                       pt2 = JTransition(1, 0, JEaseIn),
#'                       plotFn = function(pt1, pt2) {
#'                         plot(c(pt1, pt2), c(1, pt2), type = "b", xlim = c(0, 1), ylim = c(0, 1))
#'                       })
#' )
#' JAnimateScenes("animation.gif", scenes)
#'}
#'
#' @export
JAnimateScenes <- function(videoFileName, scenes, ...) {

  # Sanity check - frame rate should be the same for all scenes
  sfps <- sapply(scenes, function(s) s$fps)
  fps <- scenes[[1]]$fps
  if (!identical(rep(fps, length(scenes)), sfps)) {
    stop(sprintf("All scenes in list must have the same frame rate: found %s", paste(sfps, collapse = ", ")))
  }

  args <- list(videoFileName = videoFileName,
               nFrames = JScenesnFrames(scenes),
               plotFn = JPlotScenes(scenes),
               frameRate = fps,
               ...)

  do.call(JAnimateGIF, args)

  invisible(args)
}


###
### This could go into a vignette

# interpValues <- function(from, to, nPoints, timing) {
#   # Start with timing values
#   f <- timing(seq(0, 1, length.out = nPoints))
#   # Interpolate
#   from + (to - from) * f
# }
#
# # Function to generate a diagram demonstrating how JTransitions work
# transitionDiagram <- function(from, to, timingName, times) {
#   par(mar = c(5, 5, 4, 1) + 0.1)
#
#   n <- 100
#   x <- seq(times[1], times[2], length.out = n)
#   y <- interpValues(from, to, n, get(timingName))
#   plot(x, y, type = 'l', lwd = 2,
#        xlim = c(0, 1),
#        xlab = "Time", ylab = "",
#        main = sprintf("JTransition(from = %g, to = %g, timing = %s, times = c(%g, %g))", from, to, timingName, times[1], times[2]))
#   title(ylab = "Parameter value", mgp = c(4, 1, 0))
#   abline(h = c(from, to), col = "#0000ff40")
#   dy <- (par()$usr[4] - par()$usr[3]) / 50
#   text(0, to - dy, sprintf(" to=%g", to), adj = c(0, 1))
#   text(1, from + dy, sprintf(" from=%g", from), adj = c(1, 0))
#
#   stCol <- "brown"
#   lines(c(0, times[1]), c(from, from), col = stCol, lwd = 2)
#   abline(v = times[1], col = stCol)
#   text(times[1], from + 0.3 * (to - from), sprintf(" times[1]=%g ", times[1]), adj = 1)
#
#   etCol <- stCol
#   lines(c(times[2], 1), c(to, to), col = etCol, lwd = 2)
#   abline(v = times[2], col = etCol)
#   text(times[2], from + 0.7 * (to - from), sprintf(" times[2]=%g ", times[2]), adj = 0)
# }
#
# # transitionDiagram(2, 5, "JEaseInOut", c(0.2, 0.8))
# #
# # SinTiming <- function(x) sin(x * pi * 2.5)
# # transitionDiagram(1, 2, "SinTiming", c(0.2, 0.8))
