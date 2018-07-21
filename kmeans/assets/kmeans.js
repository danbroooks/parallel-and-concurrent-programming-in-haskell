
const setup = div => {

  const chart = (
    div.append('div')
    .attr('class', 'chart')
  );

  const controls = (
    div.append('div')
    .attr('class', 'controls')
  );

  const updateButton = (
    controls.append('button')
    .text('Update')
    .on('click', () => update(api))
  );

  const svg = (
    chart.append('svg')
    .attr('id', 'clusters')
    .attr('width', 400)
    .attr('height', 300)
  );

  const clusters = (
    svg.append('g')
  );

  const api = {
    renderClusters: data => (
      clusters
      .selectAll('circle')
      .data(data)
      .enter()
      .append('circle')
      .attr('r', ({ r }) => r)
      .attr('cx', ({ x }) => x)
      .attr('cy', ({ y }) => y)
      .attr('fill', ({ fill }) => fill)
    ),
  };

  return api;
};

const update = (page) => {
  fetch('/state')
  .then(_ => _.json())
  .then(({ title, state }) => {
    document.title = title;
    page.renderClusters(state.clusters);
    //update(page);
  });
};

document.addEventListener('DOMContentLoaded', () =>
  update(
    setup(d3.selectAll('#kmeans'))
  )
);
