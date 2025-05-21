# hours2lessons 0.1.4

* In 'mount_hours()': emphasizes the initial condition that the tuple lessons be recorded separately from the other lessons (in 'TPL', NOT in 'LSS').

* In 'mount_hours()': remove the inutil comment about the "global assignment" ("... will affect the environment of the function from which this internal function is called, NOT the global-environment").

* In 'mount_hours()': we added a control over the resulting number of gaps (teachers should not get more than two gaps).

* I decided to give up 'testthat' (I removed 'test-mount\_hours.R', which took a long time); instead, I added the exported function 'verify\_matrix()'.

* I wrapped the examples in '\donttest{}'.

* minor improvements to documentation.
