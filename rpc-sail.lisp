(in-package :tales)

(helpers:add-config-parameters
 tales-rpc-port                      12402
 tales-host                          "ec2-local-ip")

(defun start-sail ()
  (sail:start-sail-server
   "now"
   :port (helpers:config tales-rpc-port nil)
   :host (helpers:host-or-local-ip tales-host)
   :public '()
   :private ()))

(defun restart-sail ()
  (when *sails* (stop-sails))
  (sleep 0.25) ;; to get back the port
  (start-sail))
