(configuration-layer/declare-layers '(
                                      deft
                                      elfeed
                                      ;; 中文输入法 有道词典
                                      (chinese
                                        :variables
                                        chinese-default-input-method 'pinyin
                                      ;  chinese-enable-youdao-dict t
                                        )
                                      ))
