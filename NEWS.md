# hours2lessons 0.1.4

* In 'mount_hours()': emphasizes the initial condition that the tuple lessons be recorded separately from the other lessons (in 'TPL', NOT in 'LSS').

* In 'mount_hours()': remove the inutil comment about the "global assignment" ("... will affect the environment of the function from which this internal function is called, NOT the global-environment").

* In 'mount_hours()': we added a control over the resulting number of gaps (teachers should not get more than two gaps).

* I decided to give up 'testthat' (I removed 'test-mount\_hours.R', which took a long time); instead, I added the exported function 'verify\_matrix()'.

* I wrapped the examples in 'donttest{}'.

* minor improvements to documentation.

# hours2lessons 0.1.5

* Correction in 'mount_hours()': reduce line 44 to '        } # ' (by commenting the rest of this line).

# hours2lessons 1.0.0

* We require that tuples have the same number of teachers as classes; the user knows best the context of teachers' assignment to classes, so he himself must possibly set up the necessary couplings to balance the tuples of teachers by class (and not "mount_hours()", as in the previous version).

* Now the datasets used as an example are called 'dayLessons' and 'dayTuples' (instead of 'LSS' and 'Tuplaje' as in the preceding version). Now 'dayLessons' contains all lessons in the day (not only the un-tupled lessons, as older 'LSS') and 'dayTuples' contain correct tuples (in which theachers and classes are in one-to-one relation).

* Changes and additions to the vignette.
