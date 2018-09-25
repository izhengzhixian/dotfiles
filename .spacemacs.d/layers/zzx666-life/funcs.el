(define-minor-mode
  shadowsocks-proxy-mode
  :global t
  :init-value nil
  :lighter " SS"
  (if shadowsocks-proxy-mode
    (setq url-gateway-method 'socks)
    (setq url-gateway-method 'native)))


(define-global-minor-mode
  global-shadowsocks-proxy-mode shadowsocks-proxy-mode shadowsocks-proxy-mode
  :group 'shadowsocks-proxy)

