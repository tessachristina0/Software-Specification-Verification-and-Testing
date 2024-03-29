import React from 'react';
import PropTypes from 'prop-types';
import classNames from 'classnames';
import { mapToCssModules } from './utils';

const propTypes = {
  tag: PropTypes.oneOfType([PropTypes.func, PropTypes.string]),
  listTag: PropTypes.oneOfType([PropTypes.func, PropTypes.string]),
  className: PropTypes.string,
  listClassName: PropTypes.string,
  cssModule: PropTypes.object,
  children: PropTypes.node,
  'aria-label': PropTypes.string
};

const defaultProps = {
  tag: 'nav',
  listTag: 'ol',
  'aria-label': 'breadcrumb'
};

const Breadcrumb = (props) => {
  const {
    className,
    listClassName,
    cssModule,
    children,
    tag: Tag,
    listTag: ListTag,
    'aria-label': label,
    ...attributes
  } = props;

  const classes = mapToCssModules(classNames(
    className
  ), cssModule);

  const listClasses = mapToCssModules(classNames(
    'breadcrumb',
    listClassName
  ), cssModule);

  return (
    <Tag {...attributes} className={classes} aria-label={label} data-testid="breadcrumb">
      <ListTag className={listClasses}>
        {children}
      </ListTag>
    </Tag>
  );
};

Breadcrumb.propTypes = propTypes;
Breadcrumb.defaultProps = defaultProps;

export default Breadcrumb;
