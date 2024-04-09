const addButton = document.querySelector("#add");
const ingredientInput = document.querySelector("#ingredient");
const ingredientsList = document.querySelector("#ingredients");
const resultsPane = document.querySelector("#results");

addButton.addEventListener("click", function() {
    if (ingredientInput.value.length >= 2) { // don't allow ingredients with less than two characters
        const li = document.createElement("li");
        li.appendChild(document.createTextNode(ingredientInput.value));
        li.addEventListener("click", function() {
            li.remove();
            update();
        });
        ingredientsList.appendChild(li);
        ingredientInput.value = "";

        update();
    }
});

// Call the API through 'updateMatches' and redraw the results.
function update() {
    // append a single recipe to the results
    let appendResultEntry = function(item) {
        const p = document.createElement("p");
        p.appendChild(document.createTextNode(item.title));
        const ul = document.createElement("ul");
        item.ingredients.forEach(function(i) {
            const li = document.createElement("li");
            li.appendChild(document.createTextNode(i));
            ul.appendChild(li);
        });
        p.appendChild(ul);
        resultsPane.appendChild(p);
    }

    // get the ingredients as a list of strings
    let getIngredients = function() {
        let result = [];
        ingredientsList.childNodes.forEach(function(li) {
            if (li.tagName == "LI") {
                result.push(li.childNodes[0].textContent);
            }
        });
        return result;
    }

    // call the server API to get the matching recipes
    let updateMatches = async function(ingredients) {
        if (ingredients.length == 0) {
            return [];  // we don't need an API call for an empty list of ingredients
        } else {
            const response = await fetch('/api/recipe/byIngredients', {
                method: "POST",
                cache: "no-cache",
                body: JSON.stringify(ingredients)
            });
            return response.json();
        }
    }

    const ingredients = getIngredients();

    updateMatches(ingredients).then(function(response) {
        let display = response;
        if (response.length > 20) {
            display = response.slice(0, 20); // only show up to 20 matches
        }
        resultsPane.replaceChildren();
        display.forEach(appendResultEntry);
    });
}
