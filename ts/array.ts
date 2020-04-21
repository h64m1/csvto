let storage : Array<String> = []

/**
 * Elm側から配列を取得
 * @param array Elmから取得した配列
 */
function saveArray(array : Array<String>) {
	console.log('saveArray', array)
	storage = array
}
