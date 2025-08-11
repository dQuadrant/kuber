import React, { useState } from 'react';
import styles from './styles.module.css';

interface ApiEndpointProps {
  method: string;
  path: string;
  title: string;
  description: string;
  queryParams?: { name: string; type: string; description: string; optional?: boolean }[];
  requestBody?: { type: string; description: string; fields: { name: string; type: string; description: string }[] };
  responses?: { code: string; description: string }[];
}

const ApiEndpoint: React.FC<ApiEndpointProps> = ({
  method,
  path,
  title,
  description,
  queryParams,
  requestBody,
  responses,
}) => {
  const [isOpen, setIsOpen] = useState(false);

  const toggleOpen = () => {
    setIsOpen(!isOpen);
  };

  const getMethodClassName = (method: string) => {
    switch (method.toLowerCase()) {
      case 'get':
        return styles.methodGet;
      case 'post':
        return styles.methodPost;
      case 'put':
        return styles.methodPut;
      case 'delete':
        return styles.methodDelete;
      default:
        return '';
    }
  };

  return (
    <div className={styles.apiEndpoint}>
      <div className={styles.header} onClick={toggleOpen}>
        <span className={`${styles.method} ${getMethodClassName(method)}`}>{method}</span>
        <span className={styles.path}>{path}</span>
        <span className={styles.title}>{title}</span>
        <span className={`${styles.arrow} ${isOpen ? styles.arrowOpen : ''}`}>&#9660;</span>
      </div>
      {isOpen && (
        <div className={styles.content}>
          <p>{description}</p>

          {queryParams && queryParams.length > 0 && (
            <>
              <h4>Query Parameters:</h4>
              <ul>
                {queryParams.map((param, index) => (
                  <li key={index}>
                    <code>{param.name}</code> ({param.type}) {param.optional ? '(optional)' : ''}: {param.description}
                  </li>
                ))}
              </ul>
            </>
          )}

          {requestBody && (
            <>
              <h4>Request Body ({requestBody.type}):</h4>
              <p>{requestBody.description}</p>
              <ul>
                {requestBody.fields.map((field, index) => (
                  <li key={index}>
                    <code>{field.name}</code> ({field.type}): {field.description}
                  </li>
                ))}
              </ul>
            </>
          )}

          {responses && responses.length > 0 && (
            <>
              <h4>Responses:</h4>
              <ul>
                {responses.map((res, index) => (
                  <li key={index}>
                    <code>{res.code}</code>: {res.description}
                  </li>
                ))}
              </ul>
            </>
          )}
        </div>
      )}
    </div>
  );
};

export default ApiEndpoint;
