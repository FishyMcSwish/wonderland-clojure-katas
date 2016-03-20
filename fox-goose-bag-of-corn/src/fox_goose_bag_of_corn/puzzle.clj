(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]
				[[:fox :corn] [:boat :you :goose] []]
				[[:fox :corn] [:boat] [:you :goose]]
				[[:fox :corn] [:boat :you] [:goose]]
				[[:fox :corn :you] [:boat] [:goose]]
				[[:fox] [:boat :you :corn] [:goose]]
				[[:fox] [:boat] [:goose :you :corn]]
				[[:fox] [:boat :you :goose] [:corn]]
				[[:fox :you :goose] [:boat] [:corn]]
				[[:goose] [:boat :you :fox] [:corn]]
				[[:goose] [:boat] [:corn :you :fox]]
				[[:goose] [:boat :you] [:corn :fox]]
				[[:goose :you] [:boat] [:corn :fox]]
				[[] [:boat :you :goose] [:corn :fox]]
				[[] [:boat] [:you :goose :corn :fox]]
				])

(defn river-crossing-plan []
  start-pos)
