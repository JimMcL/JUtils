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

# Interpolates a single segment of points
interpValues <- function(from, to, nPoints, timing) {
  # Start with timing values
  f <- timing(nPoints)
  # Interpolate
  from + (to - from) * f
}

interpTrans <- function(trans, nFrames) {
  # Convert times to frame numbers
  keyFrames <- round(trans$times * (nFrames - 1) + 1)

  # Intialise with from values
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

timingFunctionBezier <- function(p0, p1, p2, p3, nPoints) {
  # Cubic bezier curve
  t <- seq(0, 1, length.out = 40)
  p <- matrix(c(0, 0,  p0, p1,  p2, p3,  1, 1), ncol = 2, byrow = TRUE)
  b <- bezier::bezier(t, p)
  # Linearly interpolate to get points equally spaced along the x-axis
  xout <- seq(0, 1, length.out = nPoints)
  y <- approx(b, xout = xout)

  if (F) {
    plot(b)
    lines(y)
  }

  y$y
}

timingFunctionStep <- function(stepTime, nPoints) {
  # Convert time to frame number
  stepFrame <- round(stepTime * nPoints)
  # Get value for each frame
  ifelse(seq_len(nPoints) < stepFrame, 0, 1)
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
#' @return A timing function.
#'
#' @seealso \code{\link{JTransition}}, \code{\link{JScene}}, \code{\link{JBezier}}
#'
#' @examples
#' width = JTransition(.1, 1, JEaseInOut)
#'
#' @export
JEase <- function(nPoints) timingFunctionBezier(0.25, 0.1, 0.25, 1, nPoints)
#' @rdname JEase
#' @export
JLinear <- function(nPoints) timingFunctionBezier(0, 0, 1, 1, nPoints)
#' @rdname JEase
#' @export
JEaseIn <- function(nPoints) timingFunctionBezier(0.42, 0, 1, 1, nPoints)
#' @rdname JEase
#' @export
JEaseOut <- function(nPoints) timingFunctionBezier(0, 0, 0.58, 1, nPoints)
#' @rdname JEase
#' @export
JEaseInOut <- function(nPoints) timingFunctionBezier(0.42, 0, 0.58, 1, nPoints)
#' @rdname JEase
#' @export
JBounce <- function(nPoints) timingFunctionBezier(0.175, 0.885, 0.32, 1.275, nPoints)

# Invoked by function call (additional parameters required)

#' Parameterised transition timings
#'
#' These functions allow you to create custom transition timing functions. See
#' Examples below for usage.
#'
#' @param p0,p1,p2,p3 Define the two control points (P1 & P2) of a cubic
#'   bezier curve with end points at (0, 0) and (1, 1).
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
JBezier <- function(p0, p1, p2, p3) function(nPoints) timingFunctionBezier(p0, p1, p2, p3, nPoints)
#' @rdname JBezier
#' @export
JStep <- function(time) function(nPoints) timingFunctionStep(time, nPoints)

#' Construct a JTransition
#'
#' A JTransition defines how a single parameter values changes throughout a
#' scene.
#'
#' @param from Initial parameter value.
#' @param to Final parameter value.
#' @param timing Animation timing function, such as `JEaseIn`, `JBounce`.
#' @param times Time period (start, stop) over which the transition occurs, as a
#'   proportion of the scene time. A time of 0 indicates the first frame in the
#'   scene, while 1 is the last frame. If the transition does not start at the
#'   beginning of the frame, for all earlier frames the parameter will have
#'   values `from`. Similarly, the parameter will have value `to` for any
#'   gframes after the last transition frame.
#'
#' @return List used to define how a single parameter changes within a scene.
#'
#' @seealso \code{\link{JScene}}
#'
#' @export
JTransition <- function(from, to, timing = ease, times = c(0, 1)) {
  list(from = from, to = to, times = times, timing = timing)
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
#'   documentation purposes, but the names are not used.
#' @param plotFn The parameterised plotting function. Called with one positional
#'   argument for each transition, and an optional final boolean argument,
#'   \code{add}, which is \code{TRUE} if the function should add to an existing
#'   plot rather than create a new plot. The \code{add} is only used for
#'   overlapping scenes.
#'
#' @return A list, known as a _JScene_, which can be included in a list of
#'   scenes then used to create an animation.
#'
#' @seealso \code{\link{JAnimateScenes}}
#'
#' @export
JScene <- function(duration, fps, startAfter = 0, ..., plotFn) {
  nFrames <- floor(duration * fps)
  transitions <- list(...)
  args <- lapply(transitions, interpTrans, nFrames)
  # Convert to data frame
  args <- do.call(cbind, args)
  # Does the function accept an "add" parameter?
  hasAddArg <- "add" %in% formalArgs(match.fun(plotFn))

  # Return a function that draws 1 frame
  list(nFrames = nFrames,
       offset = round(startAfter * fps),
       fps = fps, # Just so it can be checked for consistency with other scenes
       fun = function(frame, add) {
         argList <- as.list(args[frame, ])
         if (hasAddArg)
           argList <- c(argList, add = add)
         do.call(plotFn, argList)
       })
}

#' Construct a plotting function from a list of scenes.
#'
#' \code{JPlotScenes} is not usually called directly, rather it is invoked from
#' inside \code{\link{JAnimateScenes}}. Combines a list of scenes, and returns a
#' function that plots a single frame from the appropriate scene.
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
        if (sceneCount == 0)
          stop(sprintf("Internal error: couldn't find scene that starts at frame %d", frame))
        return(invisible(NULL))
      }

      startFrame <- endFrame + 1
    }
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

#' Animate a list of \code{\link{Jscene}}s.
#'
#' See \code{\link{JAnimateGIF}} for general information about creating animations from R.
#'
#' @param videoFileName Name of the GIF file to be created.
#' @param scenes A list of scene objects, each one created by calling the
#'   \code{\link{JScene}} constructor.
#' @param ... Additional parameters are passed on to \code{\link{JAnimateGIF}}.
#'
#' @return The list of arguments passed to the \code{\link{JAnimateGIF}} function (invisibly).
#'
#' @seealso \code{\link{JAnimateGIF}}, \code{\link{Jscenes}}
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
