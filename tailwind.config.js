module.exports = {
	content: ["./docs/**/*.{html,js}"],
	theme: {
		extend: {
			colors: {
				"notion-dark": "#2F3437",
			},
			screens: {
				// "device-sm": { raw: "(min-device-width: 640px)" },
			},
			boxShadow: {
				inner: "0 1px 1px 0 rgba(255, 255, 255, 0.3) inset",
			},
		},
	},
}
