// Taken from:
// https://css-tricks.com/sticky-table-of-contents-with-scrolling-active-states/

window.addEventListener('DOMContentLoaded', () => {

    const observer = new IntersectionObserver(entries => {
        entries.forEach(entry => {
            const id = entry.target.getAttribute('id');
            if (entry.intersectionRatio > 0) {
                const elem = document.querySelector(`main > nav li a[href="#${id}"]`);
                if (elem) {
                    elem.parentElement.classList.add('active');
                }
            } else {
                const elem = document.querySelector(`main > nav li a[href="#${id}"]`);
                if (elem) {
                    elem.parentElement.classList.remove('active');
                }
            }
        });
    });

    document.querySelectorAll('section[id]').forEach((section) => {
        observer.observe(section);
    });

    // Adapted from: https://stackoverflow.com/a/40475478
    document.querySelectorAll("h2, h3, h4, h5, h6").forEach(heading => {
        heading.innerHTML =
            '<a href="#' + heading.parentElement.id + '">' +
            heading.innerText +
            '</a>';
    });

});
