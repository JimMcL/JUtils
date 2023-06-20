# Test multi-scene animations

test_that("multi-scene animations", {
  # Example from README

  basePlot <- function() {
    par(mar = c(0, 0, 0, 0))
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  }

  # 30 frames per second (must be the same in all scenes)
  fps <- 30

  # Build a list of scenes
  scenes <- list(

    # Scene 1, pink square,
    JScene(1,   # Duration 1 second
           fps,

           # Define parameters that will change (transition) within the scene
           width = JTransition(.1, 1, JEaseInOut),
           height = JTransition(1, .1, JEaseInOut),
           alpha = JTransition(1, 0, JEaseIn, times = c(0.6, 1)),

           # Define a function that can plot a single frame,
           # given the approriate parameter values
           plotFn = function(width, height, alpha) {
             # Initialise the plot
             basePlot()
             # Draw the rectangle
             rect(0.5 - width / 2, 0.5 - height / 2, 0.5 + width / 2, 0.5 + height / 2,
                  density = NA, col = rgb(0.8, .05, 0.4, alpha))
           }
    ),

    # Scene 2, blue triangle
    JScene(1, # Duration 1 second
           fps,
           startAfter = -0.4, # start part-way through previous scene

           # Anime triangle width and transparency (alpha)
           width = JTransition(.1, 0.8, JEaseInOut),
           alpha = JTransition(0, 1, JEaseIn, times = c(0, 0.4)),

           plotFn = function(width, alpha, add) {
             if (!add)
               basePlot()
             polygon(x = c(0.5, 0.5 - width / 2, 0.5 + width / 2),
                     y = c(0.8, 0.2, 0.2),
                     density = NA, col = rgb(66 / 255, 200 / 255, 245 / 255, alpha))
           }
    )
  )

  # Generate the animation, loop indefinitely
  expect_error(JAnimateScenes(tf("animated.gif"), scenes, loop = 0), NA)

  # Unnamed arguments
  scenes <- list(JScene(1, fps,

                        # Animate triangle width and transparency (alpha)
                        JTransition(.1, 0.8, JEaseInOut),
                        JTransition(0, 1, JEaseIn, times = c(0, 0.4)),

                        plotFn = function(width, alpha) {
                          basePlot()
                          polygon(x = c(0.5, 0.5 - width / 2, 0.5 + width / 2),
                                  y = c(0.8, 0.2, 0.2),
                                  density = NA, col = rgb(66 / 255, 200 / 255, 245 / 255, alpha))
                        }
  ))
  # Generate the animation
  expect_error(JAnimateScenes(tf("animated.gif"), scenes), NA)


  # Wrongly named arguments
  scenes <- list(JScene(1, fps,

                        # Animate triangle width and transparency (alpha)
                        xxx = JTransition(.1, 0.8, JEaseInOut),
                        yyy = JTransition(0, 1, JEaseIn, times = c(0, 0.4)),

                        plotFn = function(width, alpha) {
                          basePlot()
                          polygon(x = c(0.5, 0.5 - width / 2, 0.5 + width / 2),
                                  y = c(0.8, 0.2, 0.2),
                                  density = NA, col = rgb(66 / 255, 200 / 255, 245 / 255, alpha))
                        }
  ))
  # Generate the animation
  expect_error(JAnimateScenes(tf("animated.gif"), scenes))

})
